---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster som hjälper till att matcha, söka och manipulera strängar inom programmering. Programmerare använder dem för att effektivisera kodning, minska fel och öka kodens läsbarhet.

## Så här gör du:
Här är ett exempel på hur du använder reguljära uttryck i C med hjälp av regex biblioteket. Villkoret är att hitta och skriva ut alla ord som börjar på bokstaven 'a'.

```C
#include <regex.h>
#include <stdio.h>

int main() {
   regex_t regex;
   int reti;
   char msgbuf[100];
   reti = regcomp(&regex, "^a", 0);
   if (reti) { fprintf(stderr, "Could not compile regex\n"); exit(1); }
   reti = regexec(&regex, "abc", 0, NULL, 0);
   if (!reti) {
      puts("Match");
   } else if (reti == REG_NOMATCH) {
      puts("No Match");
   } else {
      regerror(reti, &regex, msgbuf, sizeof(msgbuf));
      fprintf(stderr, "Regex match failed: %s\n", msgbuf);
      exit(1);
   }
   
   regfree(&regex);
   return 0;
}
```
Detta program kommer att skriva ut "Match" eftersom strängen "abc" börjar med bokstaven 'a'.

## Djupdykning
Reguljära uttryck började användas i programmering på 1950-talet och har sedan dess utvecklats och anpassats för olika programmeringsspråk. Alternativt till C, erbjuder språk som Python, JavaScript och Perl inbyggt stöd för reguljära uttryck. I C, implementerar vi dem genom att inkludera 'regex.h' biblioteket.

## Se också
1. [Tutorial on Regular Expressions in C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
2. [In-depth explanation of Regular Expressions](https://www.regular-expressions.info/tutorial.html)
3. [An alternative way of using Regular Expressions in C++](https://www.cplusplus.com/reference/regex/)
4. [Python documentation about Regular Expressions](https://docs.python.org/3/library/re.html)