---
title:                "Fish Shell: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att kunna extrahera substrängar är en användbar färdighet för programmerare. Det kan hjälpa till att effektivisera och automatisera processer, och göra det lättare att hantera data i program. Fish Shell erbjuder ett enkelt sätt att extrahera substrängar från en sträng i terminalen, vilket kan vara till stor hjälp för att lösa olika problem.

## Hur man gör det

För att extrahera substrängar i Fish Shell behöver du använda en inbyggd funktion som heter "string". Detta kommando tar två argument, den ursprungliga strängen och en specifik del av den strängen som du vill extrahera.

```Fish Shell
string SUBSTRING ORIGIN_STRING
```

För att illustrera detta, låt oss använda ett exempel där vi har en sträng med ett ord och vi vill extrahera en del av det ordet. Så här ser vår ursprungliga sträng ut:

```Fish Shell
set strängen "Jag älskar fiskar"
```

Om vi nu vill extrahera "fiskar" från den här strängen kan vi använda följande kommando:

```Fish Shell
string fiskar $strängen
```

Detta kommer att ge oss resultatet "fiskar", som är vår extraherade substräng.

## Djupdykning

För mer komplexa extractioner erbjuder Fish Shell även möjligheten att använda reguljära uttryck. Reguljära uttryck är en syntax för att matcha och manipulera textsträngar på ett flexibelt sätt.

Ett enkelt exempel på detta är om vi vill extrahera alla siffror från en sträng. Då kan vi använda detta kommando:

```Fish Shell
set strängen "Jag har 12 fiskar"
string '[0-9]+.' $strängen
```

Detta kommer att extrahera "12" från vår sträng och ge oss resultatet "12.".

## Se även

Här är några andra resurser som kan vara användbara för att lära sig mer om att extrahera substrängar i Fish Shell:

- Fish Shell dokumentation för stringkommandot: https://fishshell.com/docs/current/cmds/string.html
- En guide om reguljära uttryck i Fish Shell: https://dev.to/codefreak/regular-expressions-in-fish-shell-2n4j
- Fish Shell Cheat Sheet: https://devhints.io/fish-shell