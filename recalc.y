/*
  Christopher Holder
  FSUID - cah15n
  COP4020 - Spring 2018
  Date: April 2, 2018
*/
%{
 #include <stdio.h>
 #include <string.h> 
 #include <stdlib.h>
  int counter = 0;
  int yycolumn=1,yyline=1;
  
 struct SymbolTable{
  char name[2000];
  char type;
  int value;
  char value2[2000];
  int init; 
 };
 
 struct SymbolTable st[2000];
%}

%union{
    int sv;
    char *s;
    struct {
      int integer;
      char *str;
      int type;
    } type_id;
}
%type<type_id> expr term last termtail factor value
%type<s> idtail declaration
%type<sv> lead
%token<sv> ICONSTnumber 
%token PRINTnumber PROGRAMnumber ISnumber BEGINnumber ENDnumber 
%token VARINTnumber VARSTRnumber DIVnumber DOTnumber SEMInumber 
%token LPARENnumber MINUSnumber TIMESnumber COMMAnumber 
%token RPARENnumber PLUSnumber EQnumber 
%token<s> STRINGnumber IDnumber
 
%%

program: PROGRAMnumber IDnumber ISnumber compound {}
       ;
//Statements.
//-------------------------------------------------------
compound: BEGINnumber statement ENDnumber {} 
        ;

statement: /*empty*/ {}
         | IDnumber EQnumber value SEMInumber  
         {
           int q = checkVar($1);
           if(q == -1)//Not in symbol table.
           {
             printf("Referencing an undeclared variable on line %d. \n", yyline);
             exit(3);
           }
           else //In symbol table.
           { 
              if (st[q].type == 's')
              {
                if($3.type == 2)
                {
                  strcpy(st[q].value2,$3.str);
                  st[q].init = 1;
                }
                if($3.type == 1) //string = int
                {
                  printf("Use of a constant or variable in an incorrect context due to type %d. \n",yyline);
                  exit(5);
                }
              }//if
              if (st[q].type == 'i')
              {
                if($3.type == 1)
                {
                  st[q].value = $3.integer;
                  st[q].init = 1;
                }
                if($3.type == 2)
                {
                  printf("Use of a constant or variable in an incorrect context due to type %d. \n",yyline);
                  exit(5);
                }
              }//if
           }//else
           
         } statement
         | PRINTnumber values {printf("\n");} SEMInumber statement {}
         | declaration SEMInumber statement {}
         ;
//Print multiple values.
values: value {
        char d[2000];
        if($1.type == 1) printf("%d",$1.integer);
        if($1.type == 2) 
        {
           strncpy(d,($1.str)+1,sizeof(d));
           d[(strlen(d)-1)] = '\0';
           printf("%s",d);
        }
      } valtail
      ;

valtail: /*empty*/  {}
       | DOTnumber  value
       {
         char d[2000];
         if($2.type == 1) printf("%d",$2.integer);
         if($2.type == 2) 
         {
           strncpy(d,($2.str)+1,sizeof(d));
           d[(strlen(d)-1)] = '\0';
           printf("%s",d);
         }
       } valtail
       ;

value: STRINGnumber
     {
       $$.str = $1;
       $$.type = 2;
     }
     | expr{$$ = $1;}
     ;

//Multiple variable declarations.
//-------------------------------------------------------
declaration: VARINTnumber idtail {}  
           | VARSTRnumber idtail2 {} 
           ;
idtail: /*empty*/ {}
      | IDnumber idtail {set($1,'i');}
      | COMMAnumber IDnumber idtail {set($2,'i');} 
      ;
idtail2: /*empty*/ {}
      | IDnumber idtail2 {set($1,'s');}
      | COMMAnumber IDnumber idtail2 {set($2,'s');} 
      ;
//Expressions.
//-----------------------------------------------------------
expr: lead term {$2.integer = $1 * $2.integer;} last  {$$ = $4;}
    ;

lead: /*epsilon*/ {$$ = 1;}
    | MINUSnumber {$$ = -1;}
    ;

last: /*epsilon*/ {$$ = $<type_id>-1;}
    | PLUSnumber {} term 
    {
      if($3.type == 2) 
      {
        printf("Type error on line %d. \n", yyline);
        exit(6);
      }
      
      $3.integer += $<type_id.integer>-1; 
      $$ = $3;
    } 
    last 
    {
      $$ = $5;
    }
    | MINUSnumber {} term 
    {
      if($3.type == 2) 
      {
        printf("Type error on line %d. \n", yyline);
        exit(6);
      }
      $3.integer = $<type_id.integer>-1 - $3.integer; $$ = $3;
    } last {$$ = $5;}
    ;
term: factor {$$ = $1;} termtail {$$ = $3;}
    ;
termtail: /*epsilon*/   {$$ = $<type_id>-1;}
        | TIMESnumber {} factor 
        {
          if($3.type == 2) 
          {
            printf("Type error on line %d. \n", yyline);
            exit(6);
          }

          $3.integer *= $<type_id.integer>-1; $$ = $3;
        } termtail {$$ = $5;}
        | DIVnumber {} factor 
        {
          if($3.type == 2) 
          {
            printf("Type error on line %d. \n", yyline);
            exit(6);
          }
          $3.integer = intdiv($<type_id.integer>-1,$3.integer);
          $$ = $3;
        } 
        termtail {$$ = $5;}  
        ;
factor: ICONSTnumber {$$.integer = $1;$$.type = 1;} 
      | IDnumber 
      {
        char r = what($1);
        if (r =='i')
        {
          $$.type = 1;
          $$.integer = get($1);
        }
        else 
        {
          if (r == 's')
          {
            $$.type = 2;
            $$.str = get2($1);
          }
          else 
          {
            printf("Referencing an undeclared variable on line %d. \n", yyline);
            exit(3);
          }
        }
                    
      }     
      | LPARENnumber {} expr RPARENnumber { $$ = $3;}
      ;    
%%

int main (void) {return yyparse();}

void yyerror (char *msg) { printf("line %d: %s\n", yyline,msg);}

int yywrap(){ return 1; }

//Allows variable declaration in the symbol table.
void set(char * m,char t)
{
  int f = checkVar(m);
  if (f == -1){
    strcpy(st[counter].name,m);
    st[counter].init = 0;
    st[counter].type = t;
    st[counter].value = 0;
    strcpy(st[counter].value2,"");
    counter++;
  }
  else {
    printf("Duplicate declaration on line %d.\n", yyline);
    exit(1);
  }
}
//Obtain an int value from a valid id.
int get(char *z)
{
  int c = checkVar(z);
  if(c != -1) // If is is in the symbol table.
  {
    if(st[c].init == 0)
    {
      printf("Referencing an uninitialized variable on line %d. \n", yyline);
      exit(2);
      return -1;
    }
    else return st[c].value;
  }
  else //If it is -1
  {
    printf("Referencing an undeclared variable on line %d. \n", yyline);
    exit(3);
    return -2;
  }
}

//Obtain a char string from a valid id.
char * get2(char *z)
{
  int c = checkVar(z);
  if(c != -1) // If is is in the symbol table.
  {
    if(st[c].init == 0)
    {
      printf("Referencing an uninitialized variable on line %d. \n", yyline);
      exit(2);
      return "1";
    }
    else return st[c].value2;
  }
  else //If it is -1
  {
    printf("Referencing an undeclared variable on line %d. \n", yyline);
    exit(3);
    return "2";
  }
}

//Return index of a declared variable ,if it does not exists returns  -1.
int checkVar (char *z)
{
  int c = 0;
  while(c <= counter)
  {
    if(strcmp(z, st[c].name) == 0) return c;
    ++c;
  }
  return -1;
 
}
// Performs division.
int intdiv (int a, int b)
{
  if(b == 0)
  {
    printf("Dividing by zero on line %d.\n", yyline);
    exit(4);
  }
  return (a/b);
}
// Return character with type.
char what (char * u)
{
  int c = 0;
  while(c <= counter)
  {
    if(strcmp(u, st[c].name) == 0) return st[c].type;
    ++c;
  }
}

