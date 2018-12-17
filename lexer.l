%{
   #include <string.h>
   #include <stdio.h>
   #include "y.tab.h"

   extern int yyline, yycolumn;
%}

%%
\n      {yyline++; yycolumn=1;}
[0-9]+  {yylval.sv = atoi(yytext); yycolumn += yyleng; return(ICONSTnumber);}
print   {yycolumn += yyleng; return(PRINTnumber);}
program {yycolumn += yyleng; return(PROGRAMnumber);}
is      {yycolumn += yyleng; return(ISnumber);}
begin   {yycolumn += yyleng; return(BEGINnumber);}
end     {yycolumn += yyleng; return(ENDnumber);}
int     {yycolumn += yyleng; return(VARINTnumber);}
string     {yycolumn += yyleng; return(VARSTRnumber);}
[ \t]   {yycolumn += yyleng;}
div     {yycolumn += yyleng; return(DIVnumber);}
"."     {yycolumn += yyleng; return(DOTnumber);}
";"     { yycolumn += yyleng; return(SEMInumber);}
"("     {yycolumn += yyleng; return(LPARENnumber);}
"-"     {yycolumn += yyleng; return(MINUSnumber);}
"*"     {yycolumn += yyleng; return(TIMESnumber);}
","     {yycolumn += yyleng; return(COMMAnumber);}
")"     {yycolumn += yyleng; return(RPARENnumber);}
"+"     {yycolumn += yyleng; return(PLUSnumber);}
"="     {yycolumn += yyleng; return(EQnumber);}
\"([^"]|\\\")*\" {yylval.s = strdup(yytext); yycolumn += yyleng; return(STRINGnumber);}
[a-zA-Z]+[0-9]* {yylval.s = strdup(yytext); yycolumn += yyleng; return(IDnumber);}
.       {printf("Unknown character %s, line = %d, column = %d\n", yytext, yyline, yycolumn); yycolumn += yyleng;}
%%
