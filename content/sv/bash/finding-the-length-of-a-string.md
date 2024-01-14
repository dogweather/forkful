---
title:                "Bash: Att hitta längden på en sträng."
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom Bash-programmering. Det kan vara användbart när man behöver behandla strängar på olika sätt eller kontrollera att de är inom en viss längd. Att kunna hitta längden på en sträng är en grundläggande färdighet som kan hjälpa dig i dina Bash-projekt.

## Så här gör du

Att hitta längden på en sträng i Bash är enkelt och kan göras på flera olika sätt. Här kommer några exempel med kodblock och utskrifter.

```Bash
# Definiera en variabel med en sträng
sträng="Hej på dig"

# Använd kommandot "expr" tillsammans med "length" för att hitta längden på strängen
längd=`expr length "$sträng"`
# Utskrift: 10 eftersom det finns 10 tecken i strängen

# Du kan också använda dubbla parenteser "(( ))" och använda variabeln "$#" för att få ut längden på strängen
längd=$(( $# ))
# Utskrift: 10

# Använd kommandot "wc" tillsammans med "c" för att få ut antalet tecken i en fil
längd=`wc -c < fil.txt`
# Utskrift: 42 (om filen innehåller 42 tecken)
```

## Djupdykning

Förutom dessa exempel finns det flera andra sätt att hitta längden på en sträng i Bash. Du kan till exempel använda "printf" eller "grep" kommandon för att få ut längden på en sträng eller använda olika reguljära uttryck för att söka efter mönster i en sträng. Det är också viktigt att veta att dessa metoder fungerar på olika sätt beroende på vilken kodning som används för strängen, så det är viktigt att göra rätt undersökning för att få den korrekta längden.

## Se även

- Bash Guide for Beginners - String Operations: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html
- Bash Documentation - Built-in Commands: https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html
- How to Get Length of String in Shell Script: https://linuxhint.com/string_length_bash/