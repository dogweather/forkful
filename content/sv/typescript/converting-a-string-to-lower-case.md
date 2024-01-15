---
title:                "Omvandla en sträng till gemener"
html_title:           "TypeScript: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) kan vara användbart för att göra en sträng mer enhetlig och lättare att behandla i ett program. Det kan också användas för att jämföra strängar utan att oroa sig för skilnader i stor- och gemener.

## Så här gör du

```TypeScript
let sträng = "DEt hÄr Är en StrÄNG";
console.log(sträng.toLowerCase()); // Output: det här är en sträng
```

Det finns många olika sätt att konvertera en sträng till gemener i TypeScript, men det enklaste sättet är att använda den inbyggda metoden toLowerCase(). Denna metod tar strängen och returnerar en ny sträng med alla tecken omvandlade till gemener.

## Djupdykning

Det finns ett par saker att tänka på när man konverterar en sträng till gemener i TypeScript. För det första kan det vara viktigt att tänka på teckenkodningen för strängen. Om strängen innehåller icke-engelska tecken kan det hända att vissa kodningar inte stöds och därför inte kommer att konverteras korrekt.

För det andra, om du vill att flera olika typer av gemener ska konverteras till samma tecken, kan du använda en extern biblioteksfunktion istället för den inbyggda toLowerCase() metoden. Till exempel kan du använda biblioteket "lodash" och dess toLower() funktion som konverterar alla typer av gemener till standardgemener, oavsett språk.

## Se även

- [String.prototype.toLowerCase() Dokumentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Lodash toLower() Dokumentation](https://lodash.com/docs/4.17.11#toLower)