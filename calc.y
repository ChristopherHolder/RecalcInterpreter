%{
	#include <stdio.h>
	
	struct IDstruct
	{
		int val;
		int initialized;
		char ident[2000];	
	};

	FILE *outfile;
	int yyline = 1;
	int yycolumn = 1;
	int varIndex = 0;
	
	char string_table[20000];
	
	struct IDstruct variable_table[20000];

	int yylex();
%}

%union {
	int sv;
}

%token <sv> ICONSTnumber
%token PRINTnumber
%token PROGRAMnumber
%token ISnumber
%token BEGINnumber 
%token ENDnumber
%token VARnumber
%token DIVnumber
%token SEMInumber
%token LPARENnumber
%token MINUSnumber
%token TIMESnumber
%token COMMAnumber 
%token RPARENnumber
%token PLUSnumber
%token EQnumber
%token <sv> IDnumber

%type <sv> Factor
%type <sv> TermTail
%type <sv> Term
%type <sv> ExprTail
%type <sv> ExprHead
%type <sv> Expression

%%

P: 	PROGRAMnumber IDnumber ISnumber CompoundStatement 
	;


CompoundStatement:	BEGINnumber { print_header();} Statements ENDnumber 
	;

Statements:	Statement { fprintf(outfile, ";\n}\n");}
	|	Statement SEMInumber { fprintf(outfile, ";\n");} Statements
	;

Statement:	IDnumber EQnumber { fprintf( outfile, "%s=", &string_table[$1]); } Expression {initializeVar(&string_table[$1], $4);}

	|	PRINTnumber { fprintf(outfile, "cout << ");} Expression { printf("%d\n", $3); fprintf(outfile, " << endl"); }
	|	Declaration
	;

Declaration:	VARnumber IDnumber { fprintf(outfile, "int %s", &string_table[$2]); } IDlist {  setVariable(&string_table[$2]);}
	;

IDlist: |	COMMAnumber IDnumber {fprintf(outfile, ",%s", &string_table[$2]);} IDlist {setVariable(&string_table[$2]);}
	;


Expression:	ExprHead Term {$2 = $1 * $2;  } ExprTail {$$ = $4;}
	;

ExprHead: { $$ = 1; }
        |	MINUSnumber { fprintf(outfile, "-"); $$ = -1;}
	;

ExprTail: {$$ = $<sv>-1;}
	|	PLUSnumber { fprintf(outfile, "+"); } Term { $3 += $<sv>-1; $$ = $3;} ExprTail {$$ = $5;}
	|	MINUSnumber { fprintf(outfile, "-"); } Term { $3 = $<sv>-1 - $3; $$ = $3;} ExprTail { $$ = $5;}
	;

Term: 	Factor { $$ = $1; } TermTail {$$ = $3;}
	;

TermTail: {$$ = $<sv>-1;}
	|	TIMESnumber {fprintf(outfile, "*"); } Factor { $3 *= $<sv>-1; $$ = $3;} TermTail {$$ = $5;}
	| 	DIVnumber { fprintf(outfile, "/"); } Factor { $3 = division($<sv>-1, $3); $$ = $3;} TermTail {$$ = $5;}
	;

Factor: ICONSTnumber { fprintf(outfile, "%d", $1); $$ = $1;}
	| IDnumber	{fprintf(outfile, "%s", &string_table[$1]);$$ = getVariable(&string_table[$1]); }
	| LPARENnumber {fprintf( outfile, "("); } Expression RPARENnumber { fprintf(outfile, ")"); $$ = $3;}
	;

%%

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void print_header()
{
  if ((outfile = fopen("mya.cpp", "w")) == NULL) {
    printf("Can't open file mya.cpp.\n");
    exit(0);
  }

  fprintf(outfile, "#include <iostream>\n");
  fprintf(outfile, "#include <stdio.h>\n");
  fprintf(outfile, "using namespace std;\n");
  fprintf(outfile, "\nint main()\n");
  fprintf(outfile, "{\n");
}



int getVarID(char* var)
{
	for (int i = 0; i < varIndex; ++i)
	{
		if(strcmp(var, variable_table[i].ident) == 0)
			return i;		
	}
	return -1;
}

void setVariable( char* stringTable)
{

	if(getVarID(stringTable) != -1)
	{
		printf("Duplicate declaration on line %d.\n", yyline);
		exit(1);
	}
	strcpy(variable_table[varIndex++].ident, stringTable);
}

int getVariable(char* var)
{
	int index = getVarID(var);
	if(index == -1)
	{
		printf("Referencing an undeclared variable on line %d. \n", yyline);
		exit(2);
	} else if(!variable_table[index].initialized)
	{
		printf("Referencing an uninitialized variable on line %d. \n", yyline);
		exit(3);
	}
	return variable_table[index].val;
}

int initializeVar(char* var, int expression)
{
	int index = getVarID(var);
	variable_table[index].initialized = 1;
	variable_table[index].val = expression;
}

int division(int x, int y)
{
	if( y == 0)
	{
		printf("Dividing by zero on line %d.\n", yyline);
		exit(4);
	}
	return x/y;
}

void yyerror(const char *str)
{    
	printf("line %d: %s\n", yyline, str);
}

int main()
{
	return yyparse();		
}



