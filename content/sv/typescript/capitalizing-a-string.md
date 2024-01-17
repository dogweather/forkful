---
title:                "Idealisering av en sträng"
html_title:           "TypeScript: Idealisering av en sträng"
simple_title:         "Idealisering av en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng betyder helt enkelt att göra första bokstaven i en sträng till en stor bokstav. Programmerare gör detta för att göra strängar mer läsbara eller för att matcha specifika strängmallar.

## Hur man:

```TypeScript
const str = "hej allihopa!"

//Använda inbyggda funktioner
const kapitaliseradStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(kapitaliseradStr); //Output: "Hej allihopa!"

//Använda regex
const kapitaliseradStr2 = str.replace(/^\w/, c => c.toUpperCase());
console.log(kapitaliseradStr2); //Output: "Hej allihopa!"
```

## Djupdykning:

Att kapitalisera strängar är inget nytt koncept och har funnits sedan tidigare programmeringsspråk som C. Det finns också alternativ som att använda CSS för att ändra utseendet på text. Implementeringen av att kapitalisera en sträng kan göras på olika sätt, exempelvis genom att använda inbyggda funktioner eller regex.

## Se även:

- [CSS Text-Transform](https://www.w3schools.com/cssref/pr_text_text-transform.asp)
- [Regex JavaScript](https://www.w3schools.com/js/js_regexp.asp)