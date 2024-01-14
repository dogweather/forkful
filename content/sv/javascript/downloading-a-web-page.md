---
title:                "Javascript: Ladda ned en webbsida"
simple_title:         "Ladda ned en webbsida"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida kan vara användbart för många olika ändamål. Det kan hjälpa till att spara information från en sida, användas för webbskrapning eller för att tillhandahålla en offlinetillgång till en sida som man vill läsa senare.

## Hur man gör

För att ladda ner en webbsida i Javascript finns det flera olika sätt att göra det på. Ett sätt är att använda sig av den inbyggda `fetch()` funktionen som finns tillgänglig i moderna webbläsare. Detta är en asynkron funktion som returnerar en Promise som i sin tur kan användas för att hämta en webbsida.

För att använda `fetch()` funktionen behöver man först skapa en variabel som tilldelas själva url-addressen till sidan man vill hämta. Sedan används funktionen med variabeln som parameter och man kan sedan hantera responsen i en callback-funktion. Detta kan se ut så här:

```Javascript
var url = 'https://www.example.com';

fetch(url)
  .then(response => response.text())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

I detta exempel används `fetch()` för att hämta en webbsida och sedan konverteras responsen till textformat. Detta kan sedan loggas till konsolen för att se hur resultatet ser ut. Om det uppstår något fel kommer det att loggas till konsolen istället.

## Djupdykning

Utöver `fetch()` finns det flera andra sätt att ladda ner en webbsida i Javascript. Ett alternativ är att använda sig av en tredjepartsbibliotek som Axios eller Request. Dessa bibliotek ger fler funktioner och möjligheter för att hantera hämtning av webbsidor.

En annan viktig punkt att tänka på vid hämtning av webbsidor är säkerheten. För att undvika så kallade "cross-site scripting" attacker är det viktigt att validera och filtrera den inskickade url-addressen innan hämtning görs.

## Se även

* [MDN's dokumentation om fetch() funktionen](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
* [Axios bibliotek](https://axios-http.com/)
* [Request bibliotek](https://github.com/request/request)