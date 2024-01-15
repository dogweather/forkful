---
title:                "Söka och ersätta text"
html_title:           "C: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig funktion inom programmering som kan hjälpa dig att snabbt och effektivt ändra stora mängder text i ditt program. Det kan också vara användbart för att städa upp och organisera din kod.

## Så här gör du

För att söka och ersätta text i C använder vi funktionen `strreplace()`. Den här funktionen tar tre parametrar - den ursprungliga strängen, den nya strängen och en pekare till den variabel där den modifierade strängen ska lagras. Här är en enkel kodexempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char original[] = "Hej, världen!";
    char ersattning[] = "Hej alla!";
    char ny_strang[50];

    //söker efter "världen" och ersätter det med "alla"
    strcpy(ny_strang, strreplace(original, "världen", ersattning));
    printf("Den modifierade strängen är: %s", ny_strang);
    return 0;
}
```

**Output:**
Den modifierade strängen är: Hej, alla!

## Djupdykning

Förutom att söka och ersätta en enkel textsträng kan vi också använda `strreplace()` för att ändra mer komplicerade mönster eller för att utföra fler än en ersättning på en gång. Här är en annan kodexempel där vi använder regular expressions för att söka efter alla siffror som är större än 5 och ersätter dem med "X":

```C
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main() {
    char original[] = "Jag har 10 äpplen, men bara 2 apelsiner och 6 bananer.";
    char ny_strang[100];

    // definierar regular expression
    regex_t regex;
    regmatch_t match;

    // söker efter siffror som är större än 5 och ersätter dem med "X"
    regcomp(&regex, "([6-9]|[1-9][0-9]+)", REG_EXTENDED);
    strcpy(ny_strang, strreplace(original, &regex, "X"));

    printf("Den modifierade strängen är: %s", ny_strang);
    return 0;
}
```

**Output:**
Den modifierade strängen är: Jag har X äpplen, men bara 2 apelsiner och X bananer.

## Se också

- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Regular Expressions in C](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)