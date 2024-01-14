---
title:                "Javascript: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener kan vara användbart när man vill jämföra två strängar utan att ta hänsyn till om de är skrivna med stora eller små bokstäver.

## Hur man gör det

För att konvertera en sträng till gemener i Javascript kan du använda metoden .toLowerCase() tillsammans med din strängvariabel.

```Javascript
let sträng = "HEJ HEJ";
console.log(sträng.toLowerCase());
```

Output: hej hej

```Javascript
let sträng = "Hello World";
console.log(sträng.toLowerCase());
```

 Output: hello world

## Djupdykning

Metoden .toLowerCase() ändrar alla stora bokstäver i en sträng till gemener. Den är dock inte anpassad för specifika språk och kan därför ge olika resultat beroende på vilken teckenkodning som används. Till exempel kan bokstaven "Å" i det svenska alfabetet ändras till "å" eller "ä" beroende på teckenkodning. Detta kan leda till problem när man jämför strängar och förväntar sig exakt matchning.

## Se även

- [String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode och teckenkodning i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)