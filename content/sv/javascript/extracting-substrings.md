---
title:                "Extrahera substrängar"
html_title:           "Javascript: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
 
Att extrahera substrängar är ett vanligt verktyg som används av programmerare för att få ut specifika delar av en sträng. Det gör det möjligt att manipulera eller bearbeta data på ett mer effektivt sätt.

## Så här gör du:
 
Det finns flera sätt att extrahera substrängar i Javascript. Ett vanligt sätt är att använda metoden `substring()` som finns tillgänglig på alla strängar. Metoden tar emot två parametrar, startpositionen och slutpositionen, och returnerar en del av strängen mellan dessa positioner. Här är ett exempel på hur du kan använda `substring()`:

```Javascript
let text = "Detta är en text.";

// Extrahera "en text" från texten
let extractedText = text.substring(8, 15);

console.log(extractedText); // Output: en text
```

Om du bara vill extrahera en del av strängen från en viss position till slutet av strängen kan du använda enbart en parameter i `substring()` metoden. Här är ett exempel på det:

```Javascript
let text = "Det här är en annan text.";

// Extrahera "annan text" från texten
let extractedText = text.substring(12);

console.log(extractedText); // Output: annan text
```

En annan metod som också används för att extrahera substrängar är `slice()`. Den fungerar på samma sätt som `substring()` med den enda skillnaden att den också kan arbeta med negativa index. Här är ett exempel:

```Javascript
let text = "Substrängar är användbara.";

// Extrahera "användbara" från texten
let extractedText = text.slice(-10);

console.log(extractedText); // Output: användbara
````

## Djupdykning:
 
Extrahera substrängar är ett vanligt verktyg som funnits sedan de tidiga dagarna av programmeringsspråket Java. Det finns också andra sätt att extrahera substrängar som `substr()` och `match()` men dessa är mindre använda än `substring()` och `slice()`. Det är viktigt att notera att både `substring()` och `slice()` metoden returnerar en ny sträng utan att manipulera den ursprungliga strängen. Dessutom tar båda metoderna emot både negativa och positiva index för att ange start- och slutpositioner.

## Se också:
 
- [Javascript String Metoder](https://www.w3schools.com/js/js_string_methods.asp)
- [Substring() Metoden på Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Slice() Metoden på Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)