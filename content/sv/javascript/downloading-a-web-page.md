---
title:                "Ladda ner en webbsida"
html_title:           "Javascript: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att hämta en kopia av en webbsidas innehåll från internet och spara den på din dator. Programutvecklare gör det för att använda innehållet för olika ändamål, till exempel för att analysera eller manipulera data.

## Hur gör man:
Ett enkelt sätt att ladda ner en webbsida är att använda den inbyggda funktionen fetch() i JavaScript. Denna funktion gör en HTTP-förfrågan till en given URL och returnerar ett löfte som innehåller webbsidans innehåll. Exempelvis:

```javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data));
```

Det här koden skickar en GET-förfrågan till exempelwebbsidan, tar emot svaret och loggar innehållet i konsolen.

## Djupdykning:
Historiskt sett har programmerare använt tekniker som screen scraping för att ladda ner webbsidor. Detta innebär att man "skrapar" innehållet från en webbsida genom att ladda ner sidan och sedan söka efter specifika element som behövs. Idag används dock ofta API:er för att hämta data från webbsidor.

Alternativet till att ladda ner en webbsida är att använda en webbläsare som gör det automatiskt när du besöker en sida. Detta kan vara användbart när du vill få en kopia av en sida som är interaktiv eller dynamisk.

När du laddar ner en webbsida hämtar du ofta HTML-koden, men du kan också hämta andra filtyper som bilder eller CSS-filer som behövs för att rendera sidan korrekt.

## Se även:
- [Fetch API dokumentation](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Web scraping for beginners](https://towardsdatascience.com/web-scraping-for-beginners-9c2f6e464dcf)
- [Alternatives to downloading web pages](https://medium.com/datadriveninvestor/alternatives-to-web-scraping-4d9e08aba286)