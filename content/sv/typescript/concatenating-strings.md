---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sammanslagning av strängar (eng. concatenating strings) är när vi fäster tillsammans två eller flera strängar för att skapa en enda sträng. Programmerare gör detta för att skapa meningsfullt och dynamiskt innehåll för sina applikationer. 

## Hur man gör:

Med TypeScript kan vi göra detta på flera sätt. Här är några exempel:

```TypeScript
// Metod 1: Använda '+' operatorn 
let fornamn = "Sven";
let efternamn = "Karlsson";
let namn = fornamn + " " + efternamn;
console.log(namn); // Utdata: "Sven Karlsson"

// Metod 2: Använda '${}' Interpolation
let namn2 = `${fornamn} ${efternamn}`;
console.log(namn2); // Utdata: "Sven Karlsson"
```

## Djupgående:

Historisk sett har sammanslagning av strängar använts sedan de tidigaste programmeringsspråken. I TypeScript, rekommenderas användning av '${}' interpolationsmetoden för dess läslighet och prestanda.

Alternativen till sammanslagning inkluderar användning av `concat()` metoden, men det har visat sig vara långsammare och mer otympligt än den rekommenderade templating tekniken.

Implementationen av sammanslagning sker genom underliggande JavaScript-motorn som utför sammanslagning på runtime. 

## Se Dessutom:

- TypeScript dokumentation om strängar och templating: https://www.typescriptlang.org/play#example/strings
- En detaljerad djupgående artikel om sammanslagning och prestanda jämförelser: https://www.codeproject.com/Articles/5286158/The-Fastest-Way-to-Combine-Strings-in-JavaScript
- Mozilla's avsnitt om sammanslagning: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat