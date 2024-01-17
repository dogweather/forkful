---
title:                "Versaler och gemener i en sträng"
html_title:           "C: Versaler och gemener i en sträng"
simple_title:         "Versaler och gemener i en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "kapitalisera en sträng" är en vanlig term inom programmeringsvärlden som betyder att man gör den första bokstaven i en sträng till en stor bokstav, samtidigt som resten av bokstäverna förblir oförändrade. Detta görs vanligtvis för att göra det enklare att läsa en sträng eller för att uppfylla vissa syntaktiska krav.

## Så här:
För att kapitalisera en sträng i C-programmering, kan man använda sig av inbyggda funktioner som "toupper" eller "toupper_l". I inmatningsfältet anger man den sträng som man vill kapitalisera och funktionen returnerar sedan den kapitaliserade versionen av strängen.

```
// Kapitaliserar strängen "hej alla" 
#include <stdio.h>
#include <ctype.h>
int main() {
  char str[] = "hej alla";  
  int length = strlen(str);
  int i;
  for (i = 0; i < length; i++) {
    printf("%c", toupper(str[i]));
  }
  return 0;
}
// Output: HEJ ALLA
```

## Djupdykning:
I början av datorprogrammering, när teckenkodningarna ASCII och EBCDIC introducerades, användes olika nummer för stora och små bokstäver. Detta resulterade i att olika programvaror behandlade bokstäver på olika sätt, vilket ledde till problem när man skulle överföra data mellan olika system. För att lösa detta implementerades funktioner som "toupper" och "tolower" för att konvertera bokstäver till en standardiserad form.

## Se även:
För mer information om strängmanipulation i C-programmering, kolla in dessa länkar:
- [String library functions in C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [ASCII and EBCDIC](https://www.ibm.com/support/knowledgecenter/en/SSUKPC_11.1.0/com.ibm.xlf111.bg.doc/language_ref/casciib.html)