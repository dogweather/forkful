---
title:                "Bash: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida är en vanlig uppgift för många programmerare. Det kan användas för att skapa en lokal kopia av en webbplats, eller för att extrahera information från webbplatser för olika ändamål. Oavsett orsaken så är det en användbar färdighet att ha inom Bash-programmering.

## Hur man gör det

Att ladda ner en webbsida med Bash är en relativt enkel process som kan utföras med hjälp av verktyget "curl". Curl används för att hämta data från webbsidor via terminalen. Först behöver man hämta URL:en för den webbsida man vill ladda ner. Sedan kan man använda följande Bash-kod för att ladda ner webbsidan och spara den som en fil:

```Bash
curl <URL> -o <filnamn>
```

Detta kommer att ladda ner webbsidan och spara den med det angivna filnamnet. Om man inte anger ett filnamn så kommer webbsidan att sparas som en HTML-fil i den sökväg där koden körs.

## Deep Dive

En intressant aspekt av att ladda ner webbsidor med Bash är att man kan använda olika flaggor med "curl" för att ändra hur data hämtas. Till exempel kan man använda flaggan "-s" för att tysta utskrifter från curl, vilket kan vara användbart om man vill skapa en automatisk process för att regelbundet ladda ner en webbsida.

En annan användbar flagga är "-L" som gör att curl följer eventuella omdirigeringar, vilket kan hända om man laddar ner från en länk som leder till en annan sida.

Det finns också möjlighet att använda sed för att bearbeta den nedladdade webbsidan, till exempel för att extrahera specifika delar av information. Detta kan vara användbart för automatiserad data mining eller webbskrapning.

## Se även

För mer information och djupare förklaringar av olika curl-flaggor och användningsområden, se följande länkar:

- [Curl man-sidan](https://linux.die.net/man/1/curl) för detaljerade beskrivningar av flaggorna och deras användning.
- [Vad är curl och hur man använder det](https://www.hostinger.se/tutorials/curl-guide/) för en enkel guide om curl och hur man använder det för olika uppgifter.
- [Sed manualen](https://linux.die.net/man/1/sed) för information om hur man använder sed för att bearbeta textfiler.