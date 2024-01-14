---
title:    "C: Att skriva om en teckensträng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna ändra en sträng så att första bokstaven blir stor är en viktig funktion i C-programmering. Det låter dig formatera utdata på ett sätt som är lättläst för användaren. I den här bloggposten kommer vi att utforska hur man gör detta på ett enkelt sätt.

## Hur man gör
För att kapitalisera en sträng i C behöver du använda två inbyggda funktioner, toupper() och tolower(). Dessa funktioner ändrar bokstäverna i en sträng till antingen stora eller små bokstäver. Här är ett exempel på hur du kan använda dem i kod:

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
  char str[] = "hej alla!";
  int i;

  //kapitalisera strängen
  for (i = 0; str[i] != '\0'; i++)
  {
    str[i] = toupper(str[i]);
  }

  printf("Den kapitaliserade strängen är: %s\n", str);

  return 0;
}
```

Output:

```C
HEJ ALLA!
```

Som du kan se, använde vi en for-loop för att gå igenom varje tecken i strängen och ändra det till stora bokstäver. Sedan skriver vi ut den kapitaliserade strängen med hjälp av printf()-funktionen.

## Djupdykning
Förutom toupper() och tolower() finns det en annan funktion som är speciellt användbar för att kapitalisera strängar. Det är funktionen strupr() som finns i string.h-biblioteket. Denna funktion kapitaliserar inte bara första bokstaven i en sträng, utan alla bokstäver i strängen. Här är ett exempel på hur man använder det:

```C
#include <stdio.h>
#include <string.h>

int main()
{
  char str[] = "hej alla!";
  
  //kapitalisera hela strängen
  printf("Den kapitalgjorda strängen är: %s\n", strupr(str));

  return 0;
}
```

Output:

```C
HEJ ALLA!
```

En annan viktig sak att notera är att toupper(), tolower() och strupr() funktionerna är inte specifika för svenska bokstäver. De fungerar med alla bokstäver i det ASCII-teckenuppsättning som C använder.

## Se även
- [toupper() - C Reference](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [tolower() - C Reference](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [strupr() - C Reference](https://www.tutorialspoint.com/c_standard_library/c_function_strupr.htm)