---
title:    "Bash: Radera tecken som matchar ett mönster"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att kunna radera tecken som matchar ett visst mönster kan vara en användbar färdighet i Bash-programmering. Det kan hjälpa till att rengöra data, formatera strängar eller söka efter specifika information i en fil. Det kan också vara användbart för att utföra olika typer av manipulation på textdata.

## Hur man gör

Att radera tecken som matchar ett visst mönster i Bash är enkelt med hjälp av inbyggda kommandon och verktyg. För att illustrera detta kommer vi att använda kommandot "tr", som står för "translate" och används för att ersätta eller ta bort tecken.

För att radera alla mellanslag i en sträng kan du använda följande kommando:

```Bash
tr -d " " <<< "Hej, det här är en text med mellanslag"
```

Kommandot "tr" tar här bort alla mellanslag som matchar mönstret " " från den angivna strängen med hjälp av flaggan "-d". Resultatet blir "Hej,dethärenentextmedmellanslag".

Om du istället vill radera alla siffror kan du använda mönsteruttrycket [:digit:] och flaggan "-d" enligt följande:

```Bash
tr -d [:digit:] <<< "Hej, 123 det här är 456 en text med 789 siffror"
```

Resultatet blir då "Hej, det här är en text med siffror".

## Djupdykning

För att radera tecken som matchar ett visst mönster kan vi även använda oss av andra kommandon, såsom "sed" eller "awk". Dessa verktyg erbjuder mer avancerade mönstermatchningsmöjligheter och gör det möjligt att utföra mer komplexa manipulationer på textdata.

Till exempel kan du med hjälp av "awk" ta bort tecken som matchar både siffror och bokstäver med hjälp av följande kommando:

```Bash
awk '{gsub(/[a-zA-Z0-9]/,"")}1' <<< "Hej, 123 det här är en text med 456 siffror och bokstäver"
```

Resultatet blir "Hej, det här är en text med och". Här ersätter vi alla tecken som matchar mönstret /[a-zA-Z0-9]/ med ett tomt tecken, med hjälp av kommandot "gsub". Sedan används flaggan "1" för att skriva ut den modifierade strängen.

## Se även

- Bash-dokumentation: https://www.gnu.org/software/bash/manual/
- Bash-cheatsheet: https://devhints.io/bash
- Sed-dokumentation: https://www.gnu.org/software/sed/manual/
- Awk-dokumentation: https://www.gnu.org/software/gawk/manual/