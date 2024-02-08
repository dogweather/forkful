---
title:                "Extrahera delsträngar"
aliases:
- sv/google-apps-script/extracting-substrings.md
date:                  2024-02-01T21:53:07.739278-07:00
model:                 gpt-4-0125-preview
simple_title:         "Extrahera delsträngar"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/google-apps-script/extracting-substrings.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Extrahering av substrängar innebär att man tar en del av en sträng - i princip skapar en ny sträng från en del av en befintlig sådan. Programmerare gör detta av många anledningar, inklusive datatolkning, textmanipulering för användargränssnitt, eller bearbetning av indata för olika applikationer, vilket gör substrängsextrahering till ett mångsidigt verktyg i varje skriptarsenal.

## Hur man gör:

I Google Apps Script, som är baserat på modern JavaScript, kan extrahering av substrängar uppnås genom flera metoder, inklusive `substring()`, `substr()`, och `slice()`. Varje metod har sina nyanser, men de tjänar alla syftet att dra ut specificerade tecken från en sträng.

```javascript
// Exempel med substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Utdata: Hello

// Exempel med substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Utdata: world

// Exempel med slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Utdata: world!
```

Varje metod tar två argument: startpositionen och, förutom `slice()` som kan acceptera negativa index för att starta från slutet, slutpositionen eller antalet tecken att extrahera. Det är värt att notera att den ursprungliga strängen förblir oförändrad efter dessa operationer, eftersom de returnerar nya strängvärden.

## Djupdykning

Historiskt sett har JavaScript-metoderna för att extrahera substrängar varit en källa till förvirring på grund av deras liknande namn och funktionaliteter. Dock, i Google Apps Script och modern JavaScript, används `substring()` och `slice()` oftast, med `substr()` anses vara föråldrat. Detta är viktigt att notera för de som skriver framtidssäker kod.

Den huvudsakliga skillnaden mellan `substring()` och `slice()` är hur de hanterar negativa index; `substring()` behandlar negativa index som 0, medan `slice()` kan acceptera ett negativt index för att starta extraktionen från slutet av strängen. Detta gör `slice()` särskilt praktiskt för fall där den exakta längden på strängen kanske inte är känd eller när man behöver extrahera från slutet.

När man bestämmer vilken metod man ska använda för substrängsextraktion, handlar valet ofta om de specifika kraven för operationen (t.ex. om hantering av negativa index är fördelaktigt) och personliga eller teamkodningsstandarder. Även om det inte finns någon universell bästa praxis, kan förståelse för de subtila skillnaderna och prestandakonsekvenserna hjälpa till att fatta ett välgrundat beslut.
