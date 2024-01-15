---
title:                "Konvertera en sträng till gemener"
html_title:           "Fish Shell: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför du ska använda Fish Shell för att konvertera en sträng till små bokstäver

Att konvertera en sträng till små bokstäver är ett vanligt problem inom programmering, och ibland kan det vara svårt att hitta en smidig lösning. Fish Shell, med sin enkla syntax och inbyggda funktioner, kan göra detta till en enkel uppgift.

## Hur du gör det med Fish Shell

För att konvertera en sträng till små bokstäver i Fish Shell, kan du använda kommandot `string tolower`. Här är ett exempel på hur du kan använda detta kommando:

```Fish Shell
set my_string "HeLlO wOrLd"
string tolower $my_string
```
Detta skulle ge följande output: `hello world`

Det är också möjligt att använda en pipe för att konvertera en sträng till små bokstäver direkt från en extern källa, som t.ex. en fil:

```Fish Shell
cat my_file.txt | string tolower
```

## Djupare dykning

För att få en djupare förståelse för hur `string tolower` fungerar i Fish Shell, är det viktigt att förstå hur Fish Shell behandlar strängar. I Fish Shell är strängar standardmässigt alltid case-sensitiva, vilket innebär att den skiljer mellan små och stora bokstäver. För att undvika detta, kan du använda kommandot `string tolower` för att konvertera en sträng till en version som bara innehåller små bokstäver.

Det finns också andra metoder för att konvertera en sträng till små bokstäver i Fish Shell, som t.ex. `string tolower utf8`, som kan hantera specialtecken och unicode.

## Se även

- [Fish Shell - officiell hemsida](https://fishshell.com/)
- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Lista över inbyggda funktioner i Fish Shell](https://fishshell.com/docs/current/cmds.html)