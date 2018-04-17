CREATE OR REPLACE PACKAGE QS_PARSE_METADATA AS
	TYPE T_QS_COL IS RECORD (
		COLUMN_NAME VARCHAR2(100),
		COLUMN_TYPE	VARCHAR2(50),
		COLUMN_DIRECTIVES VARCHAR2(100)
	);

	TYPE T_QS_COLS IS TABLE OF T_QS_COL;

	TYPE T_QS_TABLE IS RECORD (
		TABLE_NAME VARCHAR2(100),
		TABLE_DIRECTIVES VARCHAR2(100),
		TABLE_COLS T_QS_COLS
	);

	TYPE T_QS_TABLES IS TABLE OF T_QS_TABLE;

	--TEXT FORMATTING
	G_IDNT CHAR(2) := '  ';
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

	PROCEDURE READ_TABLES ( T_QS_TABLES IN OUT NOCOPY T_QS_TABLES);

end QS_PARSE_METADATA;

CREATE OR REPLACE PACKAGE BODY QS_PARSE_METADATA AS
	PROCEDURE READ_TABLES ( T_QS_TABLES IN OUT NOCOPY T_QS_TABLES)
	IS
	BEGIN
		FOR TBLS IN (
			SELECT 'TB1' AS TABL_NAME, 'C1' AS COLUMN_NAME, 'NUM' as COLUMN_TYPE
			FROM DUAL
			UNION ALL
			SELECT 'TB1' AS TABL_NAME, 'C2' AS COLUMN_NAME, 'VARCHAR2' as COLUMN_TYPE
			FROM DUAL
			)
		LOOP
			
		END LOOP; 
	END;
END QS_PARSE_METADATA; 