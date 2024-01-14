---
title:                "Javascript: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å laste ned en nettside. Kanskje du vil lagre innholdet for senere referanse, eller du ønsker å analysere dataene på siden. Uansett hva grunnen din er, er det viktig å kunne laste ned en nettside effektivt for å kunne bruke informasjonen på en måte som passer dine behov.

## Slik gjør du det

Det er flere måter å laste ned en nettside på, men her vil vi se på hvordan du gjør det ved hjelp av Javascript. Først må du ha en teksteditor og en nettleser som støtter Javascript. Deretter kan du følge disse enkle stegene:

1. Åpne teksteditoren og opprett en ny fil med .html-utvidelsen.
2. I <head> taggen, legg til følgende kode: ```<script>``` og ```</script>```.
3. Inne i ```<script>``` taggen, skriv dette: ```var request = new XMLHttpRequest()```.
4. Deretter må du spesifisere hvilken nettside du vil laste ned ved å bruke ```request.open("GET", "nettside-url", true)```.
5. For å faktisk laste ned siden, bruk ```request.send()```.
6. For å se resultatet, kan du bruke ```XMLHttpRequest.responseText``` for å få ut hele siden, eller du kan bruke ```XMLHttpRequest.responseXML``` for å få ut siden i XML-format.

Et eksempel på hvordan koden kan se ut:

```
<html>
  <head>
    <title>Last ned nettside</title>
    <script>
      var request = new XMLHttpRequest();
      request.open("GET", "https://www.nettside.no", true);
      request.send();
      console.log(request.responseText);
    </script>
  </head>
  <body>
  </body>
</html>
```

## Dypdykk

Hvis du ønsker å gå dypere inn i dette emnet, er det mange ressurser og verktøy tilgjengelig. Du kan for eksempel utforske forskjellige måter å laste ned en nettside på, eller hvordan du kan behandle og manipulere dataene i den nedlastede siden. Det finnes også flere biblioteker og API-er som kan være nyttige for å automatisere denne prosessen.

En annen ting å tenke på er hvordan du kan laste ned bilder og andre ressurser som blir brukt på en nettside. Dette kan være nyttig hvis du vil lagre alle ressursene fra en nettside og bruke dem offline.

## Se også

- [Hvordan behandle data fra XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Eksempel på hvordan laste ned en nettside med Javascript](https://stackoverflow.com/questions/2704573/how-to-download-the-entire-webpage-with-the-htmlelement)
- [Automatiser nedlasting av nettsider med Puppeteer](https://github.com/puppeteer/puppeteer)