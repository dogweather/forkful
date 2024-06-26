---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:06.288146-07:00
description: "Hur man g\xF6r: I Google Apps Script \xE4r `UrlFetchApp`-tj\xE4nsten\
  \ avg\xF6rande f\xF6r att ladda ner webbinneh\xE5ll. Nedan f\xF6ljer en steg-f\xF6\
  r-steg-guide och ett enkelt\u2026"
lastmod: '2024-03-13T22:44:37.437157-06:00'
model: gpt-4-0125-preview
summary: "I Google Apps Script \xE4r `UrlFetchApp`-tj\xE4nsten avg\xF6rande f\xF6\
  r att ladda ner webbinneh\xE5ll."
title: "H\xE4mta en webbsida"
weight: 42
---

## Hur man gör:
I Google Apps Script är `UrlFetchApp`-tjänsten avgörande för att ladda ner webbinnehåll. Nedan följer en steg-för-steg-guide och ett enkelt exempel som visar hur man hämtar och loggar HTML-innehållet på en webbsida:

1. **Grundläggande hämtningsoperation:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Den här koden hämtar HTML-innehållet för example.com och loggar det. Det är en rättfram demonstration av hur man får en webbsidas källa utan några ytterligare parametrar.

2. **Hantering av omdirigeringar och HTTPS:**

För HTTPS eller hantering av omdirigeringar är koden i stort sett densamma, men överväg att implementera felhantering eller specifika alternativ för omdirigeringar:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Följer automatiskt omdirigeringar
    'muteHttpExceptions': true // Tystar möjliga undantag för att hantera dem nådigt
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Begränsningar och kvoter:**

Var uppmärksam på Google Apps Scripts kvoter; kraftig användning kan kräva felhantering för hastighetsbegränsningar.

## Fördjupning
Historiskt började nedladdning och manipulation av webbinnehåll med enkla HTTP-förfrågningar, vilket utvecklades avsevärt med introduktionen av skriptspråk. Google Apps Script möjliggör enkel utförande av sådana uppgifter inom G Suite-ekosystemet, med stöd av Googles robusta infrastruktur. `UrlFetchApp`-tjänsten är en kärnelement av denna funktionalitet, och omsluter komplexa HTTP/S-förfrågningar i ett enklare applikationsnivå gränssnitt.

Trots dess bekvämlighet kanske Google Apps Script inte alltid är det bästa verktyget för tung webbskrapning eller när komplex efterbearbetning av hämtad data krävs på grund av tidsbegränsningar och kvoter som påläggs av Google. I sådana fall kan dedikerade webbskrapningsramverk eller språk som är designade för asynkrona I/O-operationer, såsom Node.js med bibliotek som Puppeteer eller Cheerio, erbjuda mer flexibilitet och kraft.

Vidare, medan Google Apps Script är ett utmärkt verktyg för integration med Google-tjänster (som Sheets, Docs och Drive) och utförande av lättvikts datahämtningsoperationer, är det avgörande att ha i åtanke begränsningarna i dess exekveringsmiljö. För intensiva uppgifter, överväg att använda Google Cloud Functions eller Apps Scripts avancerade tjänster med externa beräkningsresurser för bearbetning.
