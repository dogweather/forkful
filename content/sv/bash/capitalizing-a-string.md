---
title:                "Bash: Stor bokstavering av en sträng"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng i Bash är en vanlig uppgift som kan vara användbar i många olika situationer. Det kan till exempel användas för att formatera användarnamn eller data som ska visas på skärmen.

## Hur man gör det

För att kapitalisera en sträng i Bash kan du använda kommandot "tr" tillsammans med en sekvens av mallar och bytesvärden. I följande kodblock ser vi hur man kapitaliserar alla bokstäver i en sträng:

```Bash
str="hej allihopa!"
str_caps=$(echo "$str" | tr "[:lower:]" "[:upper:]")
echo "$str_caps" # Resultat: HEJ ALLIHOPA!
```

Om du vill kapitalisera endast den första bokstaven i en sträng, använder du följande:

```Bash
str="hej allihopa!"
first_cap=$(echo "${str^}")
echo "$first_cap" # Resultat: Hej allihopa!
```

## Djupdykning

Förutom de grundläggande kommandona "tr" och "^" för kapitalisering av strängar finns det många andra sätt att åstadkomma samma sak i Bash. Du kan till exempel använda funktionen "printf" med mallar eller skriva en egen Bash-funktion för att hantera hanteringen av strängen.

Att kapitalisera en sträng kan också vara en del av en större uppgift, som att sortera en lista av namn i alfabetisk ordning eller att konvertera text till titelstil. Det är viktigt att förstå olika metoder för att kapitalisera strängar för att effektivt kunna lösa sådana uppgifter.

## Se även

- [Bash Manual - Kapitel 9: Kommandon för strängmanipulering](www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.htlm#Shell-Parameter-Expansion)
- [Linuxize - How to capitalize first letter of a string in Bash](www.linuxize.com/post/how-to-capitalize-first-letter-of-a-string-in-bash/)
- [Bash Hackers Wiki - String operations](http://wiki.bash-hackers.org/syntax/operations/string)