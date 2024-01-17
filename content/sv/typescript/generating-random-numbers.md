---
title:                "Generering av slumpmässiga nummer"
html_title:           "TypeScript: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer är en vanlig praxis inom programmering för att skapa oprognoserbarhet och varierande resultat. Det är användbart för spel, simuleringar och kryptering.

## Så här:
### Generera ett heltal mellan 0 och 10:
```TypeScript
Math.floor(Math.random() * 10);
```
Output: (exempel) 5

### Generera ett flyttal mellan 0 och 1:
```TypeScript
Math.random();
```
Output: (exempel) 0.823491249269

## Djupdykning:
Slumpmässig nummergenerering har funnits sedan länge, även om de ursprungliga algoritmerna var långsamma och resurskrävande. I modern tid har matematik och maskininlärning bidragit till mer effektiva och säkra metoder för att skapa slumpmässiga nummer. Alternativ till den inbyggda Math.random() funktionen inkluderar att använda dedikerade bibliotek som Crypto.getRandomValues() för bättre säkerhet.

## Se även:
* [Slumpmässighet på Wikipedia](https://sv.wikipedia.org/wiki/Slumpmässighet)
* [JavaScript random() funktion](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/Math/random)