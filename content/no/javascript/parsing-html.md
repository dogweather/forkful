---
title:                "Analysering av html"
html_title:           "Javascript: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Parsing HTML er en viktig del av webutvikling som innebærer å analysere og tolke HTML-koden på en nettside. Dette gjøres for å få frem relevant informasjon og funksjoner for brukere. Det kan være alt fra tekster og bilder til interaktive elementer som knapper og skjemaer. Parsing HTML er nødvendig for å skape et sømløst og brukervennlig nettsted.

## Hvordan: 
Det er flere måter å parse HTML på, men her er et eksempel på en enkel måte å gjøre det på ved hjelp av Javascript.

```Javascript
const parser = new DOMParser(); // Opprett et nytt DOMParser-objekt
const htmlString = "<html><body><h1>Hei, verden!</h1></body></html>"; // Her legger vi inn HTML-koden vi vil parse
const parsedBody = parser.parseFromString(htmlString, "text/html"); // Kaller på parseFromString-metoden med HTML-koden og ønsket format som argumenter
console.log(parsedBody.querySelector("h1").textContent); // Skriver ut teksten inni <h1>-elementet, som i dette tilfellet er "Hei, verden!"
```
Her bruker vi DOMParser-objektet til å konvertere HTML-koden til et HTML-dokument som kan manipuleres med Javascript. Deretter benytter vi querySelector-metoden til å finne og skrive ut teksten i <h1>-elementet.

## Deep Dive: 
Parsing HTML har vært en viktig del av webutvikling siden de tidligste dagene av internett. Først ble det vanligvis gjort ved hjelp av server-side scripting, men nå brukes det ofte klient-side teknikker som Javascript. Det finnes også alternative måter å parse HTML på, for eksempel regex-mønstre og HTML-parser-biblioteker som Cheerio og BeautifulSoup.

Javascript inneholder en innebygd HTML-parser som heter DOMParser, som vi brukte i eksempelet over. Denne parseren kan tolke HTML og XML-kode på en lik måte. Det finnes også andre måter å manipulere HTML-dokumenter på, som å bruke Document Object Model (DOM) eller Cascading Style Sheets (CSS).

## Se også: 
- https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- https://cheerio.js.org/
- https://www.crummy.com/software/BeautifulSoup/
- https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model