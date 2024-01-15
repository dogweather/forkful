---
title:                "Hämta en webbsida"
html_title:           "Javascript: Hämta en webbsida"
simple_title:         "Hämta en webbsida"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle du vilja ladda ner en webbsida? Ibland kan det vara användbart att ha en lokal kopia av en webbsida om du vill kunna komma åt den även när du inte är online, eller om du vill studera webbsidans kod.

## Så här gör du

För att ladda ner en webbsida i Javascript, kan du använda dig av det inbyggda objektet "XMLHttpRequest". Detta objektet gör det möjligt att skicka en förfrågan till en server och ta emot ett svar, vilket är precis vad vi behöver för att ladda ner en webbsida. Nedan är ett exempel på hur du kan göra det:

```Javascript
// Skapa en ny instans av XMLHttpRequest-objektet
var xhttp = new XMLHttpRequest();

// Ange vilken metod vi vill använda (i detta fall GET, vilket är standardmetoden för att ladda ner en webbsida)
xhttp.open("GET", "https://www.example.com", true);

// Skicka förfrågan till servern
xhttp.send();

// När förfrågan är klar och vi har fått ett svar, kan vi ta emot webbsidans kod genom att använda "responseText"-egenskapen.
// Detta kan vara användbart om du vill manipulera koden eller spara den till en fil.
console.log(xhttp.responseText);
```

## Djupdykning

En sak att tänka på när du laddar ner en webbsida är att de flesta moderna webbsidor använder sig av asynkron kommunikation (t.ex. asynkrona förfrågningar eller "AJAX"). Detta betyder att förfrågningarna sker i bakgrunden utan att sidan behöver laddas om. Detta kan göra det lite knepigare att ladda ner hela webbsidan, eftersom du behöver vänta tills alla förfrågningar är klara innan du kan ta emot hela sidans kod.

Ett annat sätt att ladda ner en webbsida är att använda dig av ett tredjepartsbibliotek, som t.ex. "Puppeteer". Detta är ett verktyg som låter dig interagera med en webbläsare och utföra olika åtgärder, inklusive att ladda ner en webbsida. Det är dock viktigt att komma ihåg att vissa webbsidor kanske inte tillåter automatisk nedladdning på grund av säkerhetsskäl.

## Se även

- [XMLHttpRequest-objektet](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Puppeteer](https://github.com/puppeteer/puppeteer)