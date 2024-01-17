---
title:                "Interpolering av en sträng"
html_title:           "PHP: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När man programmerar i PHP finns oftast ett behov av att kombinera textsträngar med variabler eller uttryck. Detta kallas för string interpolation. Det är ett användbart verktyg för att enklare och mer effektivt skapa dynamiska strängar.

## Så här gör du:

För att interpolera en sträng i PHP, behöver du endast placera en variabel eller ett uttryck inom en sträng, inuti dubbelcitat . I följande exempel ser du hur man interpolerar en variabel i en sträng:

```PHP
$namn = "Maria";
echo "Välkommen $namn!"; 
```

Detta kommer att resultera i följande output:

```
Välkommen Maria!
```

Det är viktigt att notera att man inte kan interpolera en variabel inuti en enkelcitat sträng. I sådana fall kommer variabeln att behandlas som en vanlig textsträng. 

Det är också möjligt att interpolera uttryck i en sträng. Ett uttryck kan vara en aritmetisk operation eller en funktion som returnerar ett värde. I följande exempel ser du hur man interpolerar ett uttryck i en sträng:

```PHP
echo "2 + 2 är lika med " . (2+2);
```

Detta kommer att resultera i följande output:

```
2 + 2 är lika med 4
```

## Djupdykning:

Interpolation har funnits i programmering världen i flera årtionden och används även i andra programmeringsspråk, som till exempel C och Ruby. I PHP finns det också en alternativ syntax för string interpolation som heter "heredoc". Detta tillåter flera rader av text och variabler att interpoleras inuti utan att behöva använda citattecken för varje rad.

Implementeringen av string interpolation i PHP är ganska enkel och det finns inga speciella funktioner som behövs för att använda det.

## Se även:

Lär mer om string interpolation: https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing.simple

Fler exempel på heredoc syntax: https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing.complex