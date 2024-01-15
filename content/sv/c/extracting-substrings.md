---
title:                "Extrahera delsträngar"
html_title:           "C: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift inom programmering, särskilt när det kommer till strängmanipulation. Genom att lära sig hur man gör detta kan du öka din förmåga att hantera och bearbeta textdata, vilket kan vara mycket användbart i många olika programvara och projekt.

## Hur man gör det

Extrahering av en delsträng innebär att man tar en del av en befintlig sträng och sparar den som en separat variabel. I C kan detta göras med hjälp av funktionen `strncpy()`, som kopierar en del av en sträng till en annan variabel.

Ett enkelt exempel på hur man kan extrahera en delsträng från en befintlig sträng ser ut såhär:

```C
char str[] = "Detta är en sträng";
char sub_str[10]; // skapar en tom variabel för delsträngen
strncpy(sub_str, str + 5, 8); // extraherar en delsträng från index 5 med längd 8
printf("Delsträngen är: %s", sub_str); // skriver ut delsträngen "är en st"
```

Detta exempel visar hur man kan använda `strncpy()` för att extrahera en del av en befintlig sträng och spara den som en separat variabel. Det är viktigt att notera att extrahering av delsträngar i C börjar på index 0.

## Djupdykning

I C finns det flera olika sätt att extrahera delsträngar, beroende på vad som behövs och vilken data som behandlas. En annan vanlig metod är användning av `strchr()` funktionen, vilket söker efter ett visst tecken i en sträng och returnerar en pekare till den första förekomsten av detta tecken.

Exempelvis kan man använda `strchr()` för att extrahera en delsträng från en sträng baserat på ett visst tecken:

```C
char str[] = "Hjälp mig att hitta ett tecken!";
char *ptr = strchr(str, 'a'); // letar efter tecknet 'a' i strängen
printf("Delsträngen är: %s", ptr); // skriver ut "agt tecken!"
```

`strchr()` kan också användas tillsammans med andra stränghanteringsfunktioner för mer komplicerade extraheringsuppgifter.

## Se även

- [C Strängmanipulering](https://www.programiz.com/c-programming/c-string-manipulation)
- [Strängfunktioner i C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)