%{
#include "y.tab.h"
%}

DIGIT [0-9]
ALPHA [a-zA-Z]
ALPHA_CAPITAL [A-Z]
ALNUM [0-9a-zA-Z]
LETTER              [a-zA-Z_]

IDENTIFIER          {LETTER}({LETTER}|{DIGIT})*


%%

"return"        return (RETURN);
"void"          return (TYPE_VOID);

"if"      return (IF);
"else"    return (ELSE);
"while"	return(WHILE);
"for"     return(FOR);
"switch"  return(SWITCH);
"case"    return(CASE);
"default" return(DEFAULT);
"do"		return(DO);
"break" return(BREAK);
"exit"			return exit_command;
[*+-/=%;~&|^(){}:]  {return yytext[0];}

"&&"		return (AND);
"||"		return(OR);
"!"			return(NOT);
"=="		return(EQ);
"!="			return(NOTEQ);
">="		return(GTE);
"<="		return(LTE);
">"			return(GT);
"<"			return(LT);
"++"		return(INC);
"--"		return(DEC);


"int"			return TYPE_INT;
"float"			return TYPE_FLT;
"string"  		return TYPE_STR;
"char"			return TYPE_CHR;
"const"   		return TYPE_CONST;
"print_sytb"   return print_symble;

[ \t\n]+        ;       /* ignore whitespace */

\-?{DIGIT}*"."{DIGIT}+      {	yylval.FLT = atof(yytext);
								return FLOATING_NUM;}
\-?{DIGIT}+				 	{	yylval.INTGR = atoi(yytext);
								return NUM;}
\'.\'						{ 	yylval.INTGR = yytext[1]-'a';
								return(CHAR_VALUE); }
{IDENTIFIER}						{	yylval.INTGR = *yytext-'a';
								return(ID);}


\"[0-9a-zA-Z ]+\" 			{ 	yylval.STRNG = yytext;
								return(STRING_VALUE); }

"//".*                	;
"/*"(.|\n)*"*/"     		;

%%