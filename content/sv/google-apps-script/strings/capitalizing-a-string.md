---
aliases:
- /sv/google-apps-script/capitalizing-a-string/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:03.719435-07:00
description: "Att g\xF6ra om en str\xE4ng s\xE5 att f\xF6rsta bokstaven \xE4r stor\
  \ och resten sm\xE5 \xE4r en vanlig metoder f\xF6r att formatera namn eller titlar.\
  \ Det inneb\xE4r att man \xE4ndrar\u2026"
lastmod: 2024-02-18 23:08:51.359972
model: gpt-4-0125-preview
summary: "Att g\xF6ra om en str\xE4ng s\xE5 att f\xF6rsta bokstaven \xE4r stor och\
  \ resten sm\xE5 \xE4r en vanlig metoder f\xF6r att formatera namn eller titlar.\
  \ Det inneb\xE4r att man \xE4ndrar\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?

Att göra om en sträng så att första bokstaven är stor och resten små är en vanlig metoder för att formatera namn eller titlar. Det innebär att man ändrar på input så att den första karaktären är stor bokstav medan resten förblir små bokstäver. Programmerare gör detta för att säkerställa datakonsistens och förbättra läsbarheten i användargränssnitt eller dokument.

## Hur gör man:

Google Apps Script, som bygger på JavaScript, tillåter flera metoder för att göra om en sträng till stor bokstav i början, även om det inte finns någon inbyggd funktion för detta. Här är ett par kortfattade exempel:

**Metod 1: Använda charAt() och slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Exempelanvändning
let result = capitalizeString('hej, världen');
console.log(result);  // Utdata: Hej, världen
```

**Metod 2: Använda ett Regex**

För de som föredrar en regex-baserad lösning för att mer elegant hantera specialfall:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Exempelanvändning
let result = capitalizeStringRegex('hej, världen');
console.log(result);  // Utdata: Hej, världen
```

Båda metoderna säkerställer att den första bokstaven i strängen är stor och resten små, lämpliga för en mängd olika tillämpningar inklusive men inte begränsat till manipulation av Google Sheets eller dokumentredigering via Apps Script.

## Fördjupning

Att göra om strängar till stor bokstav i början i Google Apps Script är okomplicerat, genom att utnyttja JavaScripts kraftfulla strängmanipuleringsförmågor. Historiskt sett har språk som Python erbjudit inbyggda metoder som `.capitalize()` för att uppnå detta, vilket lägger till ett extra steg för JavaScript- och Apps Script-programmerare. Dock, frånvaron av en inbyggd funktion i JavaScript/Google Apps Script uppmuntrar till flexibilitet och en djupare förståelse för tekniker för strängmanipulering.

För mer komplexa scenarier, såsom att göra om varje ord i en sträng till stor bokstav i början (Titelstil), kan programmerare kombinera regex-metoder med `split()` och `map()` funktioner för att bearbeta varje ord individuellt. Även om Google Apps Script inte erbjuder en direkt metod för att göra om strängar till stor bokstav i början, ger användningen av befintliga JavaScript-strängmanipuleringsmetoder stor flexibilitet, vilket tillåter utvecklare att hantera strängar effektivt enligt deras specifika behov.

I fall där prestanda och effektivitet är av yttersta vikt, är det värt att notera att direkt strängmanipulering kan vara mer prestandaeffektiv än regex, särskilt för längre strängar eller operationer inom stora loopar. Dock, för de flesta praktiska tillämpningar inom Google Apps Script, erbjuder båda metoderna pålitliga lösningar.
