---
title:                "Javascript: Utvinning av substringer"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å trekke ut substrings i kodesnutten sin? Det er mange grunner til dette, men en vanlig årsak er å håndtere og manipulere tekst. Å eksperimentere med substrings kan også gi bedre kontroll over data og gjøre det enklere å hente ut spesifikke deler av en string.

## Hvordan

For å ekstrahere en substring i JavaScript, kan du bruke metoden `substring()`. Denne metoden tar to parametere: startindeksen og sluttpindeksen, og returnerer en ny string som inneholder delen av den opprinnelige stringen du ønsker å trekke ut.

Her er et eksempel på hvordan du kan bruke `substring()`-metoden:

```Javascript
let string = "Dette er en tekst";
let substring = string.substring(10, 13);
```

I dette tilfellet vil verdien av `substring` variabelen være "tek". Det første tallet er startindeksen, som i dette tilfellet er 10, og det andre tallet er sluttpindeksen, som her er 13. Det er viktig å merke seg at sluttpindeksen ikke er inkludert i den ekstraherte substringen, så den faktiske teksten som hentes ut er fra indeks 10 til indeks 12.

Det er også verdt å nevne at både startindeksen og sluttpindeksen kan være negative tall. Hvis startindeksen er et negativt tall, vil substringsøkningen starte fra slutten av stringen. Hvis sluttpindeksen er et negativt tall, vil substringsøkningen stoppe før det angitte negative tallet, og dermed trekke ut en lengre substring. Her er et eksempel som bruker negative tall:

```Javascript
let string = "Dette er en tekst";
let substring = string.substring(-5, -1);

```

I dette tilfellet vil `substring`-variabelen inneholde "tekst", siden substringsøkningen startet fra slutten av stringen og trakk ut alle tegnene frem til det nest siste tegnet.

## Dypdykk

I tillegg til `substring()`-metoden, finnes det også andre metoder som lar deg trekke ut substrings i JavaScript. En av disse er `slice()`-metoden, som fungerer på samme måte som `substring()`, men tar også negative tall som input. En annen metode er `substr()`, som tar to parametere: startindeksen og lengden på substringen du vil trekke ut.

En annen viktig ting å huske på er at substringsøkning i JavaScript er null-basert, slik at det første tegnet i en string har indeks 0. Dette må tas i betraktning når du bestemmer startindeksen og sluttpindeksen for å unngå feil i koden din.

## Se også

Her er noen nyttige ressurser for å lære mer om å trekke ut substrings i JavaScript:

- [MDN - String.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - String.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - String.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [W3Schools - JavaScript Substrings](https://www.w3schools.com/js/js_string_substrings.asp)

Det finnes enda flere metoder og teknikker for å håndtere substrings i JavaScript, så det kan være lurt å utforske disse og finne ut hva som fungerer best for deg og ditt kodeprosjekt. Lykke til!