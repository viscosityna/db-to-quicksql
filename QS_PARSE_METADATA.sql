create or replace PACKAGE         QS_PARSE_METADATA AS

    G_DEBUG CHAR(1) := 'N';

	--TEXT FORMATTING
	G_IDNT CHAR(2) := CHR(32) || CHR(32);
    G_SPCR CHAR(1) := CHR(32); --BLANK SPACE
	G_LN_BRK CHAR(1) := CHR(10);
	--QUICK SQL COLUMN TYPES
	G_T_NUM 	VARCHAR2(5) := 'num';
	G_T_INT 	VARCHAR2(5) := 'int';
	G_T_DATE 	VARCHAR2(5) := 'date';
	G_T_TS 		VARCHAR2(5) := 'ts';
	G_T_TS_TZ 	VARCHAR2(5) := 'tstz';
	G_T_VC 		VARCHAR2(5) := 'vc';
	G_T_CLOB 	VARCHAR2(5) := 'clob';
	G_T_BLOB 	VARCHAR2(5) := 'blob';
	G_T_FILE 	VARCHAR2(5) := 'file';

	--QUICK SQL TABLE DIRECTIVES
	G_D_COMPRESSED VARCHAR2(10) := ' /compress'; --Table will be created compressed.

	--QUICK SQL COLUMN DIRECTIVES
	G_D_IDX	VARCHAR2(20) := ' /idx'; --	Will create a non unique index on the associated column.
	G_D_UNQ VARCHAR2(20) := ' /unique'; --Creates a unique constraint.
	G_D_CHK VARCHAR2(20) := ' /check'; --Creates a check constraint with with comma or white space delimited values e.g. /cc Y N
	G_D_NN 	VARCHAR2(20) := ' /nn'; --Adds a not null constraint on the column.
	G_D_BTN VARCHAR2(20) := ' /between'; --Adds a between check constraint on the column.
	G_D_FK 	VARCHAR2(20) := ' /fk'; --Foreign key references e.g. /references table_name.
	G_D_PK 	VARCHAR2(20) := ' /pk'; --	Identifies single column as table primary key.

	PROCEDURE READ_TABLES ( P_TABLES IN OUT NOCOPY T_QS_TABLES);

  FUNCTION GET_QS_OUTPUT (P_TABLES IN T_QS_TABLES)
  RETURN CLOB;

  FUNCTION GET_QS_OUTPUT
  RETURN CLOB;

END QS_PARSE_METADATA;
/
create or replace PACKAGE BODY             QS_PARSE_METADATA AS

    PROCEDURE QSDEBUG(P_MESSAGE IN VARCHAR2)
    IS
    BEGIN
        IF G_DEBUG = 'Y' THEN
            DBMS_OUTPUT.PUT_LINE(P_MESSAGE);
        END IF;
    END;
    
	FUNCTION TRANSLATE_DATA_TYPE_TO_QS(P_DATA_TYPE IN VARCHAR2)
	RETURN VARCHAR2
	IS
	BEGIN
		CASE upper(P_DATA_TYPE)
			WHEN 'NUMBER' THEN
				RETURN  G_T_NUM;
			WHEN 'INTEGER' THEN
				RETURN  G_T_INT;
			WHEN 'DATE' THEN
				RETURN  G_T_DATE;
			WHEN 'TIMESTAMP' THEN
				RETURN  G_T_TS;
			WHEN 'TIMESTAMP(6) WITH LOCAL TIME ZONE' THEN
				RETURN  G_T_TS_TZ;
			WHEN 'CLOB' THEN
				RETURN  G_T_CLOB;
			WHEN 'BLOB' THEN
				RETURN  G_T_BLOB;
			WHEN 'FILE' THEN
				RETURN  G_T_FILE;
			ELSE 
				RETURN  G_T_VC;
		END CASE; -- case P_DATA_TYPE
	END; --TRANSLATE_DATA_TYPE_TO_QS

    FUNCTION GET_COLUMN_DIRECTIVES(
        P_NULLABLE  IN VARCHAR2, 
        P_PK        IN VARCHAR2,
        P_FK        IN VARCHAR2,
        P_IDX       IN VARCHAR2,
        P_UIDX      IN VARCHAR2)
    RETURN VARCHAR2
    IS
        l_result VARCHAR2(100);
    BEGIN
    
        QSDEBUG('  P_NULLABLE: '||P_NULLABLE);
        QSDEBUG('  P_PK: '||P_PK);
        QSDEBUG('  P_FK: '||P_FK);
        QSDEBUG('  P_IDX: '||P_IDX);
        QSDEBUG('  P_UIDX: '||P_UIDX);
        IF P_NULLABLE IS NOT NULL AND P_NULLABLE = 'N' THEN
            l_result := l_result || G_SPCR || G_D_NN;
        END IF; --nn

        IF P_UIDX IS NOT NULL AND P_UIDX = 'Y' THEN
            l_result := l_result || G_SPCR || G_D_UNQ;
        END IF; --pk

        IF P_IDX IS NOT NULL AND P_IDX = 'Y' THEN
            l_result := l_result || G_SPCR || G_D_IDX;
        END IF; --pk

        IF P_PK IS NOT NULL AND P_PK = 'Y' THEN
            l_result := l_result || G_SPCR || G_D_PK;
        END IF; --pk

        IF P_FK IS NOT NULL THEN
            l_result := l_result || G_SPCR || G_D_FK || G_SPCR || P_FK;
        END IF; --pk

        RETURN l_result;
    END;

	PROCEDURE READ_TABLES ( P_TABLES IN OUT NOCOPY T_QS_TABLES )
	IS
		l_tbl_idx NUMBER := 0;
		l_col_idx NUMBER := 0;
		l_curr_tbl_rec T_QS_TABLE;
		l_curr_tbl_cols T_QS_COLS;
		l_curr_col_rec T_QS_COL;
	BEGIN
		IF P_TABLES IS NULL THEN
			P_TABLES := T_QS_TABLES();
		END IF;

		FOR tbls IN (
			SELECT object_name AS table_name
      FROM user_objects
      WHERE object_type = 'TABLE'
			)
		LOOP
			l_tbl_idx := l_tbl_idx + 1;	
			l_col_idx := 0;	
      l_curr_tbl_rec := T_QS_TABLE(tbls.table_name,null,null);
			l_curr_tbl_cols := T_QS_COLS();
      QSDEBUG('------------------------------------------------');
      QSDEBUG('READING TABLE: '||tbls.table_name);

      FOR COLS IN (
          SELECT col.table_name
          , col.column_name
          ,    col.data_type as column_type
          ,    col.data_length 
          ,    col.nullable
          ,    pk.constraint_name
          ,    DECODE(pkc.column_name, NULL , 'N', 'Y') AS pk
          ,    DECODE(idx.index_name, NULL, 'N', DECODE(idx.uniqueness, 'UNIQUE', 'N', 'Y')) idx
          ,    DECODE(idx.uniqueness, 'UNIQUE', 'Y', 'N') uidx
          ,    fk.r_table_name as fk
        FROM all_tab_cols col
        LEFT JOIN all_constraints pk
          ON col.owner = pk.owner
          AND col.table_name = pk.table_name
          AND pk.constraint_type = 'P'
        LEFT JOIN all_cons_columns pkc
          ON pk.owner = pkc.owner
          AND pk.table_name = pkc.table_name
          AND pk.constraint_name = pkc.constraint_name
          AND col.column_name = pkc.column_name
        LEFT JOIN ( SELECT  COUNT(*) OVER (PARTITION BY idxc.index_owner, idxc.index_name, idx.table_owner, idxc.table_name) col_cnt
                        ,   idxc.index_name
                        ,   idx.uniqueness
                        ,   idx.table_owner
                        ,   idxc.table_name
                        ,   idxc.column_name
                    FROM all_indexes idx
                    INNER JOIN all_ind_columns idxc
                      ON idx.owner = idxc.index_owner
                      AND idx.index_name = idxc.index_name
                      AND idx.table_owner = idxc.table_owner
                      AND idx.table_name = idxc.table_name
                    WHERE NOT EXISTS(
                      SELECT 1
                      FROM user_constraints C
                      WHERE c.constraint_name = idx.index_name
                      AND c.constraint_type = 'P'
                    )) idx
          ON col.owner = idx.table_owner
          AND col.table_name = idx.table_name
          AND col.column_name = idx.column_name
          AND idx.col_cnt = 1
        LEFT JOIN ( SELECT col_cnt
                        ,  COUNT(DECODE(col_cnt, 1, column_name, NULL)) as fk_cnt
                        ,  owner
                        ,  table_name
                        ,  column_name
                        ,  LISTAGG(r_table_name,',') WITHIN GROUP (ORDER BY r_table_name) AS r_table_name
                    FROM (SELECT COUNT(*) OVER ( PARTITION BY ccol.owner, ccol.constraint_name, ccol.table_name) AS col_cnt
                              ,  c.owner
                              ,  c.table_name
                              ,  ccol.column_name
                              ,  r.table_name as r_table_name
                          FROM all_constraints c
                          INNER JOIN all_cons_columns ccol
                            ON c.owner = ccol.owner
                            AND c.constraint_name = ccol.constraint_name
                          INNER JOIN all_constraints r
                            ON c.owner = r.owner
                            AND c.r_constraint_name = r.constraint_name
                          WHERE c.constraint_type = 'R'
                    )
                    WHERE col_cnt = 1
                    GROUP BY col_cnt, owner, table_name, column_name
        ) fk
          ON col.owner = fk.owner
          AND col.table_name = fk.table_name
          AND col.column_name = fk.column_name
          AND fk.fk_cnt = 1
        WHERE col.virtual_column = 'NO'
        AND COL.owner = user
        AND col.table_name = tbls.table_name
        ORDER BY col.table_name, col.column_id)
			LOOP
                QSDEBUG('READING COLUMN: '||COLS.COLUMN_NAME);
				l_col_idx := l_col_idx + 1;
                l_curr_col_rec := T_QS_COL(COLS.COLUMN_NAME,TRANSLATE_DATA_TYPE_TO_QS(COLS.COLUMN_TYPE),null);
                l_curr_col_rec.COLUMN_TYPE := l_curr_col_rec.COLUMN_TYPE || CASE WHEN COLS.COLUMN_TYPE = 'VARCHAR2' THEN COLS.DATA_LENGTH ELSE null END;

                l_curr_col_rec.COLUMN_DIRECTIVES := GET_COLUMN_DIRECTIVES(
                                    P_NULLABLE  => COLS.NULLABLE, 
                                    P_PK        => COLS.PK,
                                    P_FK        => COLS.FK,
                                    P_IDX       => COLS.IDX,
                                    P_UIDX      => COLS.UIDX);
                                    
                                    
				l_curr_tbl_cols.EXTEND;
				l_curr_tbl_cols(l_col_idx) := l_curr_col_rec;
			END LOOP; --columns

			l_curr_tbl_rec.TABLE_COLS := l_curr_tbl_cols;
			P_TABLES.EXTEND;
			P_TABLES(l_tbl_idx) := l_curr_tbl_rec;

		END LOOP; --tables
	END; --READ_TABLES

  FUNCTION GET_QS_OUTPUT ( P_TABLES IN T_QS_TABLES )
  RETURN CLOB
  IS
      L_RESULT CLOB;
      L_CURR_TBL T_QS_TABLE;
      L_CURR_TBL_COLS T_QS_COLS;
      L_CURR_COL T_QS_COL;
  BEGIN
      IF P_TABLES IS NULL OR P_TABLES.COUNT < 1 THEN
          RETURN NULL;
      END IF;
      L_RESULT := '------- QUICKSQL OUTPUT FOR SCHEMA: '||user||' -------';
      FOR T_IDX IN P_TABLES.FIRST .. P_TABLES.LAST LOOP
          L_CURR_TBL := P_TABLES(T_IDX);
          L_RESULT := L_RESULT || G_LN_BRK || L_CURR_TBL.TABLE_NAME || G_SPCR || L_CURR_TBL.TABLE_DIRECTIVES;
          L_CURR_TBL_COLS := P_TABLES(T_IDX).TABLE_COLS;
          FOR C_IDX IN L_CURR_TBL_COLS.FIRST .. L_CURR_TBL_COLS.LAST LOOP
              L_CURR_COL := L_CURR_TBL_COLS(C_IDX);
              L_RESULT := L_RESULT || G_LN_BRK || G_IDNT || L_CURR_COL.COLUMN_NAME || G_SPCR || L_CURR_COL.COLUMN_TYPE || G_SPCR || L_CURR_COL.COLUMN_DIRECTIVES;
          END LOOP; --C_IDX            
      END LOOP; --T_IDX
      RETURN L_RESULT;
  END;

  FUNCTION GET_QS_OUTPUT
  RETURN CLOB
  IS 
    L_TABLES T_QS_TABLES;
  BEGIN
    READ_TABLES(L_TABLES);

    RETURN GET_QS_OUTPUT(L_TABLES);
  END;
END QS_PARSE_METADATA;
/