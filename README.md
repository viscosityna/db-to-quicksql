# Databaset to QuickSQL
### Read metadata from the ORACLE database and return a string with the representation using the Quick SQL syntax.



## Installation

1) Install the types:

```
$ sqlplus <USER>@ORCLPDB1

SQLcl: Release 17.4.0 Production on Mon Apr 23 11:09:34 2018

Copyright (c) 1982, 2018, Oracle.  All rights reserved.

Connected to:
Oracle Database 12c Standard Edition Release 12.2.0.1.0 - 64bit Production


SQL>@QS_TYPES.SQL

````

2) Install the package:

```
$ sqlplus <USER>@ORCLPDB1

SQLcl: Release 17.4.0 Production on Mon Apr 23 11:09:34 2018

Copyright (c) 1982, 2018, Oracle.  All rights reserved.

Connected to:
Oracle Database 12c Standard Edition Release 12.2.0.1.0 - 64bit Production


SQL>@QS_PARSE_METADATA.sql

```

## Usage

Run this as the *OWNER* of the objects you want to convert to **QuickSQL**

```
DECLARE
    l_quicksql CLOB;
    procedure print_clob( p_clob in clob )
    as
         l_offset number default 1;
    begin
       loop
         exit when l_offset > dbms_lob.getlength(p_clob);
         dbms_output.put( dbms_lob.substr( p_clob, 255, l_offset ) );
         l_offset := l_offset + 255;
       end loop;
       dbms_output.put_line('');
   end;
BEGIN
    l_quicksql := QS_PARSE_METADATA.GET_QS_OUTPUT;
    print_clob(l_clob);
END;
```

This tool is in beta state, **Not Production Ready** yet.
