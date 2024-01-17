---
title:                "Sammanslagning av strängar"
html_title:           "Bash: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringkonkatenering är processen att sammanslå flera strängar till en enda sträng. Detta är användbart för programmerare eftersom det ger möjlighet att bygga dynamiska strängar som kan innehålla variabler och andra dynamiska värden.

## Såhär gör du:
För att sammanslå två strängar i Bash kan vi använda kommandot `printf` tillsammans med `%s` argument. Exempelvis:
```Bash
str1="Det var en"
str2="gång"

printf "%s %s" $str1 $str2
```
Detta kommer att producera strängen "Det var en gång" som output. Observera att de två strängarna separeras med ett mellanslag för att skapa en tydlig sammanslagningspunkt.

För att sammanslå fler än två strängar, kan vi använda `$()` syntaxen för att skapa en variabel som innehåller den sammanslagna strängen. Till exempel:
```Bash
str1="Hej"
str2="dit"
str3="!"

result=$(printf "%s %s%s" $str1 $str2 $str3)
```
Variabeln `result` kommer nu att innehålla strängen "Hej dit!".

## Djupdykning:
Historiskt sett har konkatenering varit en viktig del av programmering, då det tillåter skapandet av dynamiska strängar som kan anpassas baserat på olika variabler och inmatningar. Alternativen för att sammanslå strängar i Bash inkluderar också dubbel citaionsmarkörer (" ") eller användning av `echo` kommandot.

I bakgrunden använder Bash `printf` kommandot C-programmerare med samma syntax och funktionalitet. Det finns också flera andra språk som erbjuder liknande funktioner för att sammanslå strängar, till exempel Python's `.join()` metod eller PHP's `.` operator.

## Se även:
- [Bash String Manipulation](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-bash)
- [C Concatenate Strings](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Python Join Method](https://www.geeksforgeeks.org/python-join-method/)
- [PHP Concatenation Operator](https://www.w3schools.com/php/php_operators.asp)