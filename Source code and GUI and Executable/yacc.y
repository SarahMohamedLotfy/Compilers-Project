%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
 
void reset();
void declare_initalize(int id, int type_,int check, int func);
void declare_only(int id, int type_);
void assign_only(int id, int type_, int diff);
void declare_const(int id, int type);
void func_var(int id, int type);
void new_func(int id, int type);
void func_call(int id);

void calc_lowp(char*);
void calc_highp(char*);
void cond_lowp(char*);
void cond_highp(char*);
void switch_test ();
void open_brace();
void close_brace ();
void switch_expr();
void print_symbol_table();
void check_ret(int id,int a);


void check_Semantic_func(int id, int _type);


char * get_type(int type);
char * get_type_fun(int type);

int new_scope();
int exit_scope();
int opened_scopes = 0;
int nesting_arr[100];
int nesting_last_index = -1;
int exitFlag=0;
FILE* fp;
FILE* fo;
int next_reg = 1; // The register number to be written in the next instruction
int next_cond_reg = 11;
int first_for = 1;
int is_first = 1; // check if is the first operation for consistent register counts
int is_first_cond = 1;
int after_hp = 0; // a high priority operation is done
int after_hp_cond = 0; // a high priority operation is done
int var_dec[26];
int fun_dec[26];
int is_const[26];// for each variable store 1 if it constant
int var_scope[26]; // a scope number for each variable
int param_scope[26];
int func_scope[26];
int cscope = 0;
int next_case = 0;
int current_scope = 0;
int var_init[26];
int var_type[26];
int fun_type[26];
int fun_id[26];

int var_fun_dec[26];
int var_fun_type[26];
int var_fun_fun[26];
int func = 0;
%}

%start statement
%union {int INTGR; char * STRNG; float FLT; char CHR;}
%token IF ELSE ELSEIF FOR WHILE SWITCH CASE DO BREAK DEFAULT
%token TYPE_INT TYPE_FLT TYPE_STR TYPE_CHR TYPE_CONST PRINT_SYM_TAB print_symble
%token <INTGR> ID
%token <INTGR> NUM
%token <FLT> FLOATING_NUM
%token <CHR> CHAR_VALUE
%token <STRNG> STRING_VALUE
%token RETURN
%token TYPE_VOID
%token exit_command
%type <INTGR> math_expr
%type <INTGR> math_expr_assignment
%type <INTGR>math_expression_init
%type <INTGR> mult_div_expr
%type <INTGR> math_element
%type <INTGR> math_element_NUM
%type <FLT> math_element_FLT
%type <CHR> math_element_CHR
%type <STRNG> math_element_STR
%type <INTGR> math_element_ID

%type <STRNG> functionCall argList argListCall count dataType countt data
%left '~' '^' '&' '|'
%left '+' '-'
%left '*' '/'
%left AND OR NOT EQ NOTEQ GTE LTE GT LT INC DEC


%%

statement	: variable_declaration_statement ';' {reset();}
			| assign_statement ';' {reset();}
			| constant_declaration_statement ';' {reset();}
			| conditional_statement {reset();}
			| math_expr ';' {reset();}
			| increment_decrement ';'  {reset();}
			| exit_command ';' {exit(EXIT_SUCCESS);}
			| statement variable_declaration_statement ';' {reset();}
			| statement assign_statement ';' {reset();}
			| statement constant_declaration_statement ';' {reset();}
			| statement conditional_statement {reset();}
			| statement math_expr ';' {reset();}
			| open_brackets statement close_brackets statement {;}
			| statement open_brackets statement close_brackets {;}   
			| function_statement  {reset();}
			| statement function_statement  {reset();}
			| functionCall  {reset();}
			| statement functionCall  {reset();}
			| statement exit_command ';' {exit(EXIT_SUCCESS);}
			| print_symble ';' {print_symbol_table();}
			| statement print_symble ';' {print_symbol_table();}
			|  //empty
			;


function_statement :
			TYPE_INT ID {check_Semantic_func($2,1);} func_open_brace '(' argList ')' '{'  statement  returnFunc ';'func_close_brace {exit_scope();fprintf(fp,"%c ENDP\n", $2+'a');}
	        |TYPE_FLT ID {check_Semantic_func($2,2);} func_open_brace'('argList ')' '{' statement returnFunc ';' func_close_brace  {exit_scope();fprintf(fp,"%c ENDP\n", $2+'a');}
	        |TYPE_CHR ID {check_Semantic_func($2,3);} func_open_brace'(' argList')' '{' statement returnFunc ';' func_close_brace {exit_scope();fprintf(fp,"%c ENDP\n", $2+'a');}
	        |TYPE_STR ID {check_Semantic_func($2,4);} func_open_brace'(' argList')' '{' statement returnFunc ';' func_close_brace  {exit_scope();fprintf(fp,"%c ENDP\n", $2+'a');}
			|TYPE_VOID ID  {check_Semantic_func($2,5);}  func_open_brace '(' argList')' '{'  statement  func_close_brace     { fprintf(fp,"%c ENDP\n", $2+'a');}

			;

func_open_brace :  {reset();open_brace();}
func_close_brace : '}' {reset();close_brace();}

functionCall: ID  {func_call($1);} '(' argListCall ')' ';'
            ;

returnFunc:   RETURN {check_ret(5,0);}
            | RETURN NUM  {check_ret(1,0); }
			| RETURN ID             {check_ret($2,1); }
			| RETURN FLOATING_NUM {check_ret(2,0); }
			| RETURN CHAR_VALUE  {check_ret(3,0); }
			| RETURN STRING_VALUE {check_ret(4,0); }
			;
			


argList:
	 TYPE_INT  ID {fprintf(fp,"POP_INT %c\n", $2+'a');} count  {func_var($2 ,1); declare_only($2,1);
																			  fprintf(fp,"MOV R%d, %c\n",next_reg++,$2+'a');
																			  }
	|TYPE_FLT  ID {fprintf(fp,"POP_FLT %c\n", $2+'a');} count  {func_var($2 ,2);declare_only($2,2);
																			   fprintf(fp,"MOV R%d, %c\n",next_reg++,$2+'a');
																			  }
	|TYPE_CHR  ID {fprintf(fp,"POP_CHR %c\n", $2+'a');} count {func_var($2 ,3);declare_only($2,3);
																			  fprintf(fp,"MOV R%d, %c\n",next_reg++,$2+'a');
																			  }
	|TYPE_VOID ID {fprintf(fp,"POP_VOID %c\n", $2+'a');} count {func_var($2 ,5);}
	|TYPE_STR  ID {fprintf(fp,"POP_STR %c\n", $2+'a');} count  {func_var($2 ,4);declare_only($2,4);
																			  fprintf(fp,"MOV R%d, %c\n",next_reg++,$2+'a');
																			  }
	|
	;


data:        NUM {fprintf(fp,"PUSH_INT %d \n", $1);}
            | ID {  
			if (var_type[$1] ==1) fprintf(fp,"PUSH_INT %c \n", $1+'a');
			if (var_type[$1] ==2) fprintf(fp,"PUSH_FLT %c \n", $1+'a');
			if (var_type[$1] ==3) fprintf(fp,"PUSH_CHR %c \n", $1+'a');
			if (var_type[$1] ==4) fprintf(fp,"PUSH_STR %c \n", $1+'a');
			}

			|FLOATING_NUM {fprintf(fp,"PUSH_FLT %f \n", $1);}
			| CHAR_VALUE  {fprintf(fp,"PUSH_CHR %c \n", $1+'a');}
	        ;
dataType:
	TYPE_INT  	 {fprintf(fp,"POP_INT");}
	|TYPE_FLT 	{fprintf(fp,"POP_FLT");}
	|TYPE_CHR {fprintf(fp,"POP_CHR");}
	|TYPE_VOID{fprintf(fp,"POP_VOID");}
	|TYPE_STR{fprintf(fp,"POP_STR");}
	;            
argListCall: data  countt 
        |
        ;


countt: ',' data countt 
    |
	;
	
count: ',' TYPE_INT ID {fprintf(fp,"POP_INT %c\n", $3+'a');} count {declare_only($3,1); 
																			  fprintf(fp,"MOV R%d, %c\n",next_reg++,$3+'a');}
    |',' TYPE_FLT ID {fprintf(fp,"POP_FLT %c\n", $3+'a');} count {declare_only($3,1); fprintf(fp,"MOV R%d, %c\n",next_reg++,$3+'a');}
    |',' TYPE_CHR ID {fprintf(fp,"POP_CHR %c\n", $3+'a');} count {declare_only($3,1);fprintf(fp,"MOV R%d, %c\n",next_reg++,$3+'a');}
    |',' TYPE_STR ID {fprintf(fp,"TYPE_STR %c\n", $3+'a');} count {declare_only($3,1);fprintf(fp,"MOV R%d, %c\n",next_reg++,$3+'a');}
	|',' TYPE_VOID ID {fprintf(fp,"POP_VOID %c\n", $3+'a');} count
    |
	;

conditional_statement :
			if_statement {;}
			|while_loop {;}
			|for_loop {;}
			|do_while {;}
			|switch_statement{;}
			;


switch_statement: SWITCH '(' math_expr ')' {switch_expr();new_scope();} switch_body;

switch_body:
			open_brackets cases {int tmp = exit_scope(); fprintf(fp,"label%d%c:\nlabel%d:\n",tmp,'a'-1+next_case,tmp);} close_brackets
			|open_brackets cases default {int tmp = exit_scope();fprintf(fp,"label%d%c:\nlabel%d:\n",tmp,'a'-1+next_case,tmp);} close_brackets


cases: CASE {if(next_case>0){fprintf(fp,"label%d%c:\n",nesting_arr[nesting_last_index],'a'-1+next_case);}
							next_case++;}
math_expr  {switch_test();}':' statement case_break{;}
	                        |cases cases {;}
			                ;
case_break: 
			| BREAK ';' {fprintf(fp,"JMP label%d\n",nesting_arr[nesting_last_index]);}

default: DEFAULT ':' statement {;}
         | DEFAULT ':' BREAK ';'{fprintf(fp,"JMP label%d\n",nesting_arr[nesting_last_index]);}
		 ;

do_while: DO '{' {fprintf(fp,"label%d:\n",new_scope()); open_brace();} statement '}' {close_brace();} WHILE '('condition')' ';' {fprintf(fp,"JT R10,label%d\n",exit_scope());}
for_loop:
			FOR '(' assign_statement for_first_semi_colon condition for_second_semi_colon assign_statement ')'for_open_brac statement for_close_brac {;}
for_first_semi_colon: ';' {fprintf(fp,"MOV RF,0\n");
								fprintf(fp,"label%d:\n",new_scope());reset();}
for_second_semi_colon : ';' {fprintf(fp,"JF R10, label%da\n",nesting_arr[nesting_last_index]);
								fprintf(fp,"CMPE RF,0\n");
								fprintf(fp,"JT R10, label%db\n", nesting_arr[nesting_last_index]);}
for_open_brac : '{' {fprintf(fp,"label%db:\n",nesting_arr[nesting_last_index]);
							fprintf(fp,"MOV RF,1\n");
							open_brace();
							reset();}
for_close_brac : '}' {fprintf(fp,"JMP label%d\n",nesting_arr[nesting_last_index]);
							fprintf(fp,"label%da:\n",exit_scope());
							close_brace();
						}


while_loop :
			WHILE {fprintf(fp,"label%d:\n",new_scope());} '(' condition ')' while_open_brace statement while_close_brace {;}
			;
while_open_brace : '{' {fprintf(fp,"JF R10, label%da\n",nesting_arr[nesting_last_index]);reset();open_brace();}
while_close_brace : '}' {fprintf(fp,"JMP label%d\n",nesting_arr[nesting_last_index]);
						 fprintf(fp,"label%da:\n",exit_scope());reset();close_brace();}


if_statement :
			IF '(' condition ')'if_open_brace statement if_close_brace {;}
			| IF '(' condition ')'if_open_brace statement if_close_brace Else_last statement if_close_brace {;}
			| IF '(' condition ')'if_open_brace statement if_close_brace ELSE if_statement {;}
			;
Else_last : ELSE '{' {fprintf(fp,"JT R10, label%d\n",new_scope());open_brace();reset();}
if_open_brace : '{' {fprintf(fp,"JF R10, label%d\n",new_scope());open_brace();reset();}
if_close_brace : '}' {fprintf(fp,"label%d:\n",exit_scope());close_brace();};

condition :'(' condition ')' {;}
		    | condition OR comparing_condition {cond_lowp("OR");}
			| condition AND comparing_condition  {cond_lowp("AND");}
			| NOT condition              {fprintf(fp,"NOT R10\n");}
			| comparing_condition {;}
			;


comparing_condition :
			math_expr EQ math_expr {cond_highp("CMPE");}
			| math_expr NOTEQ math_expr {cond_highp("CMPNE");}
			| math_expr GTE math_expr {cond_highp("CMPGE");}
			| math_expr GT math_expr {cond_highp("CMPG");}
			| math_expr LTE math_expr {cond_highp("CMPLE");}
			| math_expr LT math_expr {cond_highp("CMPL");}
		// hoon
			;

math_expr	:'('math_expr')'											{$$=$2;}
			|math_expr '+' mult_div_expr    { calc_lowp("ADD"); }
			| math_expr '-' mult_div_expr    		{ calc_lowp("SUB"); }
		    | '~' math_expr 													{
																												$$ = ~$2;
																												if(after_hp)
																													fprintf(fp,"NOT R4\n");
																												else
																													fprintf(fp,"NOT R%d\n",next_reg-1);
																											}
			| math_expr '|' mult_div_expr				{ calc_lowp("OR"); }
			| math_expr '&' mult_div_expr				{ calc_lowp("AND"); }
			| math_expr '^' mult_div_expr				{ calc_lowp("XOR"); }
			|mult_div_expr												{	$$=$1;}
			;

mult_div_expr:		mult_div_expr '*' math_element		{ calc_highp("MUL"); }
						|mult_div_expr '/' math_element	 { calc_highp("DIV"); }
						|math_element					{ $$=$1; }
						;

math_element:	NUM			  				{$$=$1;
																fprintf(fp,"MOV R%d, %d\n",next_reg++ ,$1);}
				| FLOATING_NUM					{$$=$1;
																fprintf(fp,"MOV R%d, %f\n",next_reg++,$1);}
				| ID 										{$$=$1;
																	if(var_dec[$1] == 1){
																		if(var_init[$1] == 1){
																			$$=$1;
																			fprintf(fp,"MOV R%d, %c\n",next_reg++,$1+'a');
																		} /*else {
																			fprintf(fp,"Semantic error: %c is not set\n", $1+'a');
																		}*/
																	} else {
																		fprintf(fp,"Semantic error: %c is not declared\n", $1+'a');
																	}
																}
				| '('math_expr')'				{$$=$2;}
				;

assign_statement: math_expr_assignment
                  |ID '=' math_element_NUM	{	assign_only($1,1,-1);}
				  | ID '=' math_element_FLT	{	assign_only($1,2,-1);}
				  | ID '=' math_element_CHR	{	assign_only($1,3,-1);}
				  | ID '=' math_element_STR	{	assign_only($1,4,-1);}
				  | ID '=' math_element_ID	{	assign_only($1,-1,$3);}
				  | ID '=' ID { assign_only($1,6,$3);} '(' argListCall ')'
				 ;


math_expr_assignment  : ID '='  '('math_expr')'		{$$=$4; assign_only($1,-1,-1);}
                                                    
			            |ID '='  math_expr '+' mult_div_expr   {calc_lowp("ADD");assign_only($1,-1,-1);}    
			            |ID '='  math_expr '-' mult_div_expr     {calc_lowp("SUB");assign_only($1,-1,-1);} 		
		                | ID '='  '~' math_expr 		 {  $$ = ~$4;
																												if(after_hp)
																													fprintf(fp,"NOT R4\n");
																												else
																													fprintf(fp,"NOT R%d\n",next_reg-1);
																													
																												assign_only($1,-1,-1);	
																													}										
																												
																											
			            | ID '='  math_expr '|' mult_div_expr {calc_lowp("OR");assign_only($1,-1,-1);}				
			            |ID '='  math_expr '&' mult_div_expr		{calc_lowp("AND");assign_only($1,-1,-1);}		
			            |ID '='  math_expr '^' mult_div_expr	{calc_lowp("XOR");assign_only($1,-1,-1);}				
			            |ID '='  mult_div_expr '*' math_element	{calc_highp("MUL");assign_only($1,-1,-1);}	
						|ID '=' mult_div_expr '/' math_element	{calc_highp("DIV");assign_only($1,-1,-1);}
			            ;


math_element_NUM:	NUM			  		{$$=$1;
									 fprintf(fp,"MOV R%d, %d\n",next_reg++ ,$1);}
				;

math_element_FLT: FLOATING_NUM		{$$=$1;
									 fprintf(fp,"MOV R%d, %f\n",next_reg++,$1);}
				;
math_element_CHR: CHAR_VALUE		{$$=$1;
									 fprintf(fp,"MOV R%d, '%c'\n",next_reg++,$1+'a');}
				;
math_element_STR: STRING_VALUE		{$$=$1;
									 fprintf(fp,"MOV R%d, %s\n",next_reg++,$1);}
				;
math_element_ID : ID 				{$$=$1;
										if(var_dec[$1] == 1){
											if(var_init[$1] == 1){
													$$=$1;
													fprintf(fp,"MOV R%d, %c\n",next_reg++,$1+'a');
											} 
										} else {
											  fprintf(fp,"Semantic error: %c is not declared\n", $1+'a');
											  }
								    }
				 ;


increment_decrement: ID DEC
                    | ID INC
					;
math_expression_init	:'('math_expr')'											{$$=$2;}
			|math_expr '+' mult_div_expr    { calc_lowp("ADD"); }
			| math_expr '-' mult_div_expr    		{ calc_lowp("SUB"); }
		    | '~' math_expr 													{
																												$$ = ~$2;
																												if(after_hp)
																													fprintf(fp,"NOT R4\n");
																												else
																													fprintf(fp,"NOT R%d\n",next_reg-1);
																											}
			| math_expr '|' mult_div_expr				{ calc_lowp("OR"); }
			| math_expr '&' mult_div_expr				{ calc_lowp("AND"); }
			| math_expr '^' mult_div_expr				{ calc_lowp("XOR"); }
			|mult_div_expr '*' math_element												{	$$=$1;}
			|mult_div_expr '/' math_element	 { calc_highp("DIV"); }
			;


variable_declaration_statement:
	TYPE_INT ID 	{ 	declare_only($2,1);}
	|TYPE_FLT ID	{ 	declare_only($2,2);}
	|TYPE_CHR ID	{ 	declare_only($2,3);}
	|TYPE_STR ID    { 	declare_only($2,4);}
	|TYPE_INT ID '=' math_expression_init	{ 	declare_initalize($2,1,-1, -1);}
	|TYPE_FLT ID '=' math_expression_init	{ 	declare_initalize($2,2,-1,-1);}

	|TYPE_INT ID '=' math_element_ID	{ 
	                                     if (var_dec[$2] == 0)
	                                            declare_initalize($2,1, $4,-1);
										  else
										  fprintf(fp,"Semantic Error : %c is already declared\n", $2 + 'a');
										  }

    
	|TYPE_FLT ID '=' math_element_ID
										  { 
	                                     if (var_dec[$2] == 0)
	                                            declare_initalize($2,2,$4,-1);
										  else
										  fprintf(fp,"Semantic Error : %c is already declared\n", $2 + 'a');
										  }


	|TYPE_INT ID '=' math_element_NUM	{ 	declare_initalize($2,1,-1,-1);}
	|TYPE_FLT ID '=' math_element_FLT	{ 	declare_initalize($2,2,-1,-1);}

	|TYPE_CHR ID '=' ID	{
	                           if(var_dec[$2] == 0) {
							      var_dec[$2] = 1;
									var_type[$2] = 3;
									var_scope[$2] = cscope;
									is_const[$2] = 0;

							        if (var_dec[$4] == 1){
									if (var_type[$4]== 3){
									  if(var_init[$4] == 1){
									
									var_init[$2] = 1;
									fprintf(fp,"MOV %c,'%c'\n",$2+'a',$4+'a');
									}
									else
									   fprintf(fp,"Semantic Error : %c is not initialized\n", $4 + 'a');
									}
									else{
									fprintf(fp,"Semantic Error : %c cannot be assigned to this type\n", $2 + 'a');
									}
									}
									else{
										fprintf(fp,"Semantic Error : %c is not declared\n", $4 + 'a');
									}
								 } else {
									 fprintf(fp,"Semantic Error : %c is an already declared variable\n", $2 + 'a');
										}
								        }

	|TYPE_STR ID '=' ID	{
	                           if(var_dec[$2] == 0) {
							   var_dec[$2] = 1;
									var_type[$2] = 3;
									var_scope[$2] = cscope;
									is_const[$2] = 0;
							        if (var_dec[$4] == 1){
									if (var_type[$4]== 4){
									if(var_init[$4] == 1){

									var_init[$2] = 1;
									fprintf(fp,"MOV %c,%s\n",$2+'a',$4);
									}
									else
									   fprintf(fp,"Semantic Error : %c is not initialized\n", $4 + 'a');
									}
									else{
									fprintf(fp,"Semantic Error : %c cannot be assigned to this type\n", $2 + 'a');
				  fprintf(fp,"Semantic Error : %c cannot assign to this variable type because %c of type %s  and %c of type %s \n", $2 + 'a', $2+'a',get_type(var_type[$2]) ,$4+'a',get_type(var_type[$4]) );

									}
									}
									else{
										fprintf(fp,"Semantic Error : %c is not declared\n", $4 + 'a');
									}
								 } else {
									 fprintf(fp,"Semantic Error : %c is an already declared variable\n", $2 + 'a');
										}
								        }

	|TYPE_CHR ID '=' CHAR_VALUE	{if(var_dec[$2] == 0) {
									var_dec[$2] = 1;
									var_type[$2] = 3;
									var_scope[$2] = cscope;
									is_const[$2] = 0;
									var_init[$2] = 1;
									fprintf(fp,"MOV %c,'%c'\n",$2+'a',$4+'a');
								 } else {
									 fprintf(fp,"Semantic Error : %c is an already declared variable\n", $2 + 'a');
										}
								        }

	|TYPE_CHR ID '=' FLOATING_NUM { fprintf(fp,"Semantic Error : char can not be assigned a floating number\n");}
	|TYPE_STR ID '=' STRING_VALUE {if(var_dec[$2] == 0) {
									var_dec[$2] = 1;
									var_type[$2] = 4;
									var_scope[$2] = cscope;
									is_const[$2] = 0;
									var_init[$2] = 1;
									fprintf(fp,"MOV %c, %s \n",$2+'a',$4);
								 } else {
									 fprintf(fp,"Semantic Error : %c is an already declared variable\n", $2 + 'a');
										}
								        }

	|TYPE_STR ID '=' FLOATING_NUM { fprintf(fp,"Semantic Error : string can not be assigned a floating number\n");}
	|TYPE_INT ID '=' ID   { declare_initalize($2,1,2,$4);} '(' argListCall ')' 
	|TYPE_FLT ID '=' ID{ declare_initalize($2,2,2,$4);} '('argListCall ')' 
	|TYPE_CHR ID '=' ID { declare_initalize($2,3,2,$4);} '(' argListCall')' 
	|TYPE_STR ID '=' ID {declare_initalize($2,4,2,$4);} '(' argListCall')' 
	;


constant_declaration_statement: TYPE_CONST TYPE_INT ID '=' math_expr { declare_const($3,1);}
                                | TYPE_CONST TYPE_FLT ID '=' math_expr	{declare_const($3,2);}
	                            | TYPE_CONST TYPE_CHR ID '=' CHAR_VALUE	{
																		if(var_dec[$3] == 0) {
																		var_dec[$3] = 1;
																		var_type[$3] = 3;
																		var_scope[$3] = cscope;
																		is_const[$3] = 1;
																		var_init[$3] = 1;
																	    fprintf(fp,"MOV %c,'%c'\n",$3+'a',$5+'a');
																			} else {
																					fprintf(fp,"Semantic Error : %c is an already declared variable\n", $3 + 'a');
																				   }
																		}
								| TYPE_CONST TYPE_STR ID '=' STRING_VALUE	{
																		if(var_dec[$3] == 0) {
																		var_dec[$3] = 1;
																		var_type[$3] = 4;
																		var_scope[$3] = cscope;
																		is_const[$3] = 1;
																		var_init[$3] = 1;
																	    fprintf(fp,"MOV %c, %s \n",$3+'a',$5);
																			} else {
																					fprintf(fp,"Semantic Error : %c is an already declared variable\n", $3 + 'a');
																				   }
																		}
								;	

open_brackets: '{' { open_brace(); } ;
close_brackets: '}' { close_brace(); };
%%

int main(int argc, char* argv[]) { 
   
    FILE* outputFile;
    FILE* outputFileSym;
    outputFile = fopen("compiledoutput.txt", "w");
    outputFileSym = fopen("symboltable.txt", "w");
    
   
    fp = outputFile;
    fo = outputFileSym;
    yyparse();
    fclose (fp);
    fclose (fo);
    return 0;
}
int yyerror(char* s)
{
  fprintf(stderr, "%s\n",s);
}
int yywrap()
{
  return(1);
}


void print_symbol_table()
{
	fprintf(fo,"Symbol Table:\n=============\n");
	fprintf(fo,"Variables:\n=============\n");
	fprintf(fo,"Symbol\t\tType\t\tInitialized\t\tConstant\t\t\t\n");
	int i;
	for (i = 0 ; i < 26 ; i ++){
		if(var_dec[i] == 1 && var_scope[i] == 1)
		{
			fprintf(fo,"%c\t\t%s\t\t",i+'a',get_type(var_type[i]));
			if(var_init[i] == 1)
				fprintf(fo,"true\t\t\t");
			else fprintf(fo,"false\t\t\t");
			if(is_const[i] == 1)
				fprintf(fo,"true\t\t\t");
			else fprintf(fo,"false\t\t\t");
			fprintf(fo,"\n");
		}
	}
	fprintf(fo,"Functions:\n=============\n");
	fprintf(fo,"Symbol\t\tType\t\t\t\n");
	for (i = 0 ; i < 26 ; i ++){
		if(fun_dec[i] == 1)
		{
			fprintf(fo,"%c\t\t%s\t\t\n",i+'a',get_type_fun(fun_type[i]));
		}
	}
}
char * get_type_fun(int type){
	switch(type){
	case 1:
		return "int";
	case 2:
		return "float";
	case 3:
		return "char";
    case 4:
		return "string";
	case 5:
		return "void";
	}
}


void func_var(int id,int type){
	var_fun_dec[id] = 1;
	var_fun_type[id] = type;
	var_fun_fun[id] = func;	
}

void new_func(int id,int type){
	fun_dec[id] = 1; 
	fun_type[id] = type;
}

void func_call(int id){
	if(fun_dec[id] == 1){
		fprintf(fp,"CALL %c \n", id+'a');
	}
	else{
		fprintf(fp,"Semantic Error : %c is not declared\n", id + 'a');
	}
}
void check_ret(int id, int val){
	if(val == 1){
		if(var_dec[id] == 1){
			if(var_type[id]+'0' == fun_type[fun_id[func]]+'0'){
				fprintf(fp,"RET \n");
			}
			else{
				/* printf("Semantic error : %c \n" , fun_id[func]+'a');
				fprintf(fp,"Semantic error : %c \n" , fun_type[fun_id[func]]+'0');
				fprintf(fp,"Semantic error : %c \n" , var_type[id]+'0'); */
				fprintf(fp,"Semantic error : %c is not the same return type of function", id + 'a');
			}
		}
		else{
			fprintf(fp,"Semantic error : %c is not declared in this scope", id + 'a');
		}
	}
	else{
		/* printf("Semantic error : %c " , fun_type[fun_id[func]]);
		fprintf(fp,"Semantic error : %c " , id); */
		if(id == fun_type[fun_id[func]]){
			fprintf(fp,"RET \n");
		}
		else{
			fprintf(fp,"Semantic error : %c is not the same return type of function", id + 'a');
		}
	}
}

void check_Semantic_func(int id, int _type){
	if( fun_dec[id] == 0) {
		fun_dec[id] = 1;
		fun_type[id] = _type;
		func++; 
		fun_id[func]=id;
		fprintf(fp,"PROC %c \n", id+'a');

		} else {
		fprintf(fp,"Semantic Error : %c is an already declared function\n", id + 'a');
	}
}
char * get_type(int type){
	if(type == 1)
		return "int";
	if(type == 2)
		return "float";
	if(type == 3)
		return "char";
    if(type == 4)
		return "string";
}
void calc_lowp (char * op) {
	/*$$ = $1 + $3;*/
	if(is_first){
		fprintf(fp,"%s R0,R%d,R%d\n", op, --next_reg ,--next_reg );
		is_first=0;
	}
	else{
		if(after_hp){
			fprintf(fp,"%s R0,R%d,R4\n",op, --next_reg);
			after_hp = 0;
		}
		else{
			fprintf(fp,"%s R0,R%d,R0\n",op, --next_reg);
		}
		}
}

void calc_highp (char * op) {
	if(!after_hp){
		fprintf(fp,"%s R4,R%d,R%d\n", op, --next_reg ,--next_reg );
		after_hp = 1;
		is_first = 0;
	}
	else{
		fprintf(fp,"%s R4,R%d,R4\n", op, --next_reg );
	}
}

void cond_lowp (char * op) {
fprintf(fp,"%s R10,R10,R14\n",op);
}

void cond_highp (char * op) {
	if(!after_hp_cond){
		fprintf(fp,"%s R10,R%d,R%d\n", op, --next_reg ,--next_reg );
		after_hp_cond = 1;
		is_first_cond = 0;
	}
	else{
		fprintf(fp,"%s R14,R%d,R%d\n", op, --next_reg, --next_reg );
	}
}
void switch_test () {
	if(is_first){
		fprintf(fp,"CMPE R10,RS,R%d\n", --next_reg );
		is_first=0;
	}
	else{
		if(after_hp){
			fprintf(fp,"CMPE R10,RS,R4\n", --next_reg);
		}
		else{
			fprintf(fp,"CMPE R10,RS,R0\n", --next_reg);
		}
		}
		fprintf(fp,"JF R10,label%d%c\n",nesting_arr[nesting_last_index],'a'-1+next_case);
		reset();
}

void declare_only(int id,int type_)
{
	if(var_dec[id] == 0) {
	var_dec[id] = 1;
	var_type[id] = type_;
	var_scope[id] = cscope;
	var_init[id] = 0;
	is_const[id] = 0;
	} else {
		fprintf(fp,"Semantic Error : %c is an already declared variable\n", id + 'a');
	}
}
void assign_only(int id, int type_, int diff){

      if(var_dec[id] == 1) {
		if (is_const[id] == 0) {
		
		if (type_ == 6)
		{
		   if(fun_dec[diff] == 1){
		      if (fun_type[diff]== var_type[id] ){
			   var_init[id] = 1;
		       fprintf(fp,"CALL %c \n", diff+'a');
			   }
			   else
				   fprintf(fp,"Semantic Error : %c cannot assign to this this function type because %c of type %s  and %c of type %s \n", id + 'a', id+'a',get_type(var_type[id]) ,diff+'a',get_type_fun(var_type[diff]) );
	        }
	        else{
		        fprintf(fp,"Semantic Error : %c is not declared\n", func + 'a');
	        }
		}

		if (type_ ==-1 && diff == -1)
		  {
		  if (var_type[id] == 1 || var_type[id] == 2)
		  {
		     var_init[id] = 1;
			 if(is_first){
			 
			    fprintf(fp,"MOV %c,R%d\n",id+'a',--next_reg);
				}else{
					if(after_hp)
						fprintf(fp,"MOV %c,R4\n",id+'a');
					else
						fprintf(fp,"MOV %c,R0\n",id+'a');
				  }
			}
			 else
			 {
			    fprintf(fp,"Semantic Error : %c cannot assign to mathematical expression\n", id + 'a');
			 }
			 }
			
			
			 if (type_ == -1 && diff != -1)
			  {
			  if (var_dec[diff] ==1){
			     if (var_type[id] == var_type[diff])
		        {
				if (var_init[diff] == 1){
			      var_init[id] = 1;
			     if(is_first){
				fprintf(fp,"MOV %c,R%d\n",id+'a',--next_reg);
				}else{
					if(after_hp)
						fprintf(fp,"MOV %c,R4\n",id+'a');
					else
						fprintf(fp,"MOV %c,R0\n",id+'a');
				     }
					 }
					 else
					 fprintf(fp,"Semantic Error : %c is not initialized\n", diff + 'a');
				}
			else
			      fprintf(fp,"Semantic Error : %c cannot assign to this variable type because %c of type %s  and %c of type %s \n", id + 'a', id+'a',get_type(var_type[id]) ,diff+'a',get_type(var_type[diff]) );
				
				}
				}
			
			if (type_ != -1 && diff == -1){
			   if (var_type[id] == type_)
			   {
			   var_init[id] = 1;
			   if(is_first){
				   fprintf(fp,"MOV %c,R%d\n",id+'a',--next_reg);
				 }else{
					if(after_hp)
						fprintf(fp,"MOV %c,R4\n",id+'a');
					else
						fprintf(fp,"MOV %c,R0\n",id+'a');
				      }
				}
			else{ 
				 fprintf(fp,"Semantic Error : %c cannot assign to this variable type because %c of type %s  and %c of type %s \n", id + 'a', id+'a',get_type(var_type[id]) ,diff+'a',get_type(var_type[diff]) );
				}
			}





			} else {
				fprintf(fp,"Semantic Error : %c is a constant\n", id + 'a');
			}
	} else {
		fprintf(fp,"Semantic Error : %c is not declared\n", id + 'a');
	}
}

void switch_expr(){
	if(is_first){
		fprintf(fp,"MOV RS,R%d\n",--next_reg);
		}else{
			if(after_hp)
				fprintf(fp,"MOV RS,R4\n");
			else
				fprintf(fp,"MOV RS,R0\n");
		}
	}

void declare_const(int id, int _type)
{
	if(var_dec[id] == 0) {
			var_dec[id] = 1;
			var_type[id] = _type;
			var_scope[id] = cscope;
			var_init[id] = 1;
			is_const[id] = 1;
			if(is_first){
				fprintf(fp,"MOV %c,R%d\n",id+'a',--next_reg);
		}else{
			if(after_hp)
				fprintf(fp,"MOV %c,R4\n",id+'a');
			else
				fprintf(fp,"MOV %c,R0\n",id+'a');
			}
	} else {
		fprintf(fp,"Semantic Error : %c is an already declared variable\n", id + 'a');
	}
}
void declare_initalize(int id, int _type, int check, int func){
    int function_found = 0;
	if(var_dec[id] == 0) {
		var_dec[id] = 1;
		var_type[id] = _type;
		var_scope[id] = cscope;
		is_const[id] = 0;
		if (check ==2) {
		   if(fun_dec[func] == 1){
		      if (fun_type[func]== _type){
		       function_found = 1;
		      fprintf(fp,"CALL %c \n", func+'a');
			   }
			   else
				   fprintf(fp,"Semantic Error : %c cannot assign to this this function type because %c of type %s  and %c of type %s \n", id + 'a', id+'a',get_type(_type) ,func+'a',get_type_fun(fun_type[func]) );
	        }
	        else
		        fprintf(fp,"Semantic Error : %c is not declared\n", func + 'a');
	        }

	    int idSuitable =0;
        if (check != 2 && check !=-1)
		{
	                                      if (var_dec[check] == 1){
									         if (var_type[check]== _type){
											    if (var_init[check] == 1 )
												   idSuitable = 1;
												else
												    fprintf(fp,"Semantic Error : %c is not intialized\n", check + 'a');
											  }
									       else
				   fprintf(fp,"Semantic Error : %c cannot assign to this this function type because %c of type %s  and %c of type %s \n", id + 'a', id+'a',get_type(_type) ,check+'a',get_type(var_type[check]) );

									         }
									    else
										fprintf(fp,"Semantic Error : %c is not declared\n", check + 'a');			
		}
		if (idSuitable == 1 || check == -1 || function_found ==1){
		    var_init[id] = 1;
		    if (check !=2){
		     if(is_first){
			     fprintf(fp,"MOV %c,R%d\n",id+'a',--next_reg);
		       }else{
			      if(after_hp)
				     fprintf(fp,"MOV %c,R4\n",id+'a');
			      else
				   fprintf(fp,"MOV %c,R0\n",id+'a');
			        }
			           }
			}

	} 
	else {
		fprintf(fp,"Semantic Error : %c is an already declared variable\n", id + 'a');
	}
}
void reset()
{
	next_reg = 1;
	is_first = 1;
	after_hp = 0;
	is_first_cond = 1;
	after_hp_cond = 0;
	fprintf(fp,"\n");
}
void open_brace() {
	cscope++;
}

void close_brace () {
    int i;
	for (i = 0; i < 26; i++) {
			if (var_scope[i] == cscope ) {
				var_scope[i] = -1;
				var_dec[i] = 0;
			}
	}
	cscope--;
}


int new_scope()
{
	opened_scopes ++;
	nesting_last_index ++;
	nesting_arr[nesting_last_index] = opened_scopes;
	return opened_scopes;
}
int exit_scope()
{
	int tmp = nesting_arr[nesting_last_index];
	nesting_last_index --;
	return tmp;
}

