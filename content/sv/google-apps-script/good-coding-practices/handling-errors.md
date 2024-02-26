---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:01.803816-07:00
description: "Felhantering i Google Apps Script handlar om att f\xF6rutse, f\xE5nga\
  \ och svara p\xE5 undantag eller fel som uppst\xE5r under skriptets k\xF6rning.\
  \ Programmerare\u2026"
lastmod: '2024-02-25T18:49:35.783439-07:00'
model: gpt-4-0125-preview
summary: "Felhantering i Google Apps Script handlar om att f\xF6rutse, f\xE5nga och\
  \ svara p\xE5 undantag eller fel som uppst\xE5r under skriptets k\xF6rning. Programmerare\u2026"
title: Hantera fel
---

{{< edit_this_page >}}

## Vad & Varför?

Felhantering i Google Apps Script handlar om att förutse, fånga och svara på undantag eller fel som uppstår under skriptets körning. Programmerare implementerar detta för att skydda skript mot oväntade fel, vilket säkerställer smidigare, användarvänliga applikationer som kan hantera eller logga fel på ett elegant sätt utan plötsliga krascher.

## Hur man gör:

Google Apps Script, som är baserat på JavaScript, tillåter oss att använda det traditionella `try-catch`-uttrycket för felhantering, tillsammans med `finally` om städning krävs oavsett framgång eller fel.

```javascript
function myFunction() {
  try {
    // Kod som kan orsaka ett fel
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Cell A1 är tom.");
    }
    Logger.log(data);
  } catch (e) {
    // Kod för felhantering
    Logger.log("Fel: " + e.message);
  } finally {
    // Städkod, utförd oavsett om ett fel uppstod eller inte
    Logger.log("Funktionen slutförd.");
  }
}
```

Exempel på utskrift utan fel:
```
[Värde i cell]
Funktionen slutförd.
```

Exempel på utskrift med ett fel (om man antar att A1 är tom):
```
Fel: Cell A1 är tom.
Funktionen slutförd.
```

Google Apps Script stöder också att kasta anpassade fel med `Error`-objektet och att fånga specifika feltyper vid behov. Dock gör avsaknaden av avancerad felfördelning det nödvändigt att förlita sig på felmeddelanden för specifika detaljer.

## Fördjupning

Historiskt sett har felhantering i skriptspråk som JavaScript (och därmed Google Apps Script) varit mindre sofistikerad än i vissa kompilerade språk, vilka erbjuder funktioner som detaljerade undantagshierarkier och omfattande felsökningsverktyg. Google Apps Scripts modell är relativt enkel, vilket använder JavaScripts `try-catch-finally`-paradigm. Denna enkelhet är i linje med språkets design för att snabbt utveckla och distribuera applikationer i små till medelstora skala inom Googles ekosystem, men det kan ibland begränsa utvecklare som hanterar komplexa felscenarier.

I mer komplexa applikationer kompletterar programmerare ofta Google Apps Scripts inbyggda felhantering med anpassad loggning och rapportering av fel. Detta kan inkludera att skriva fel till ett Google-kalkylblad för revision eller att använda tredjepartstjänster för loggning genom Google Apps Scripts URL Fetch-tjänster för att skicka felinformation ut ur skriptmiljön.

Även om Google Apps Script kanske ligger efter språk som Java eller C# när det gäller inbyggd komplexitet och kapacitet för felhantering, gör dess integration med Googles tjänster och enkelheten hos `try-catch-finally`-tillvägagångssättet det till ett kraftfullt verktyg för utvecklare att snabbt automatisera uppgifter och skapa integrationer inom Googles ekosystem. Utvecklare från andra bakgrunder kan finna att utmaningen inte ligger i att bemästra komplexa mönster för felhantering, men i att kreativt utnyttja det som finns tillgängligt för att säkerställa att deras skript är robusta och användarvänliga.
