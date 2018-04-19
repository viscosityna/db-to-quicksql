create or replace PACKAGE QS_PARSE_METADATA AS

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
    
    FUNCTION GET_QS_OUTPUT (P_TABLES IN T_QS_TABLES) RETURN CLOB;

end QS_PARSE_METADATA;
/
create or replace PACKAGE BODY QS_PARSE_METADATA AS
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
			WHEN 'TIMESTAMP WITH TIME ZONE' THEN
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

	PROCEDURE READ_TABLES ( P_TABLES IN OUT NOCOPY T_QS_TABLES)
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
		FOR TBLS IN (
			SELECT 'TB1' AS TABLE_NAME
			FROM DUAL
			UNION ALL
			SELECT 'TB2' AS TABLE_NAME
			FROM DUAL
			)
		LOOP
			l_tbl_idx := l_tbl_idx + 1;	
			l_col_idx := 0;	
            l_curr_tbl_rec := T_QS_TABLE(TBLS.TABLE_NAME,null,null);
			l_curr_tbl_cols := T_QS_COLS();
            DBMS_OUTPUT.PUT_LINE('READING TABLE: '||TBLS.TABLE_NAME);
			FOR COLS IN (
				SELECT 'C1' AS COLUMN_NAME, 'NUM' as COLUMN_TYPE
				FROM DUAL
				--WHERE TABLE_NAME = TBLS.TABLE_NAME
				UNION ALL
				SELECT  'C2' AS COLUMN_NAME, 'VARCHAR2' as COLUMN_TYPE
				FROM DUAL
				)
			LOOP
                DBMS_OUTPUT.PUT_LINE('READING COLUMN: '||COLS.COLUMN_NAME);
				l_col_idx := l_col_idx + 1;
                l_curr_col_rec := T_QS_COL(COLS.COLUMN_NAME,TRANSLATE_DATA_TYPE_TO_QS(COLS.COLUMN_TYPE),null);
                l_curr_col_rec.COLUMN_DIRECTIVES := G_D_NN;
				l_curr_tbl_cols.EXTEND;
				l_curr_tbl_cols(l_col_idx) := l_curr_col_rec;
			END LOOP; --columns
			l_curr_tbl_rec.TABLE_COLS := l_curr_tbl_cols;
			P_TABLES.EXTEND;
			P_TABLES(l_tbl_idx) := l_curr_tbl_rec;
		END LOOP; --tables
	END; --READ_TABLES
    
    FUNCTION GET_QS_OUTPUT (P_TABLES IN T_QS_TABLES)
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
END QS_PARSE_METADATA; 
/