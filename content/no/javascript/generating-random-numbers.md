---
title:                "Javascript: Generering av tilfeldige tall"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en svært nyttig ferdighet som alle som jobber med programmering bør ha. Enten det er for å lage spill, utføre dataanalyse eller lage tilfeldige elementer i en nettside, kan tilfeldige tall være en viktig del av programmet ditt.

## Hvordan

For å generere tilfeldige tall i Javascript, kan vi bruke Math.random() funksjonen. Denne funksjonen returnerer et tilfeldig desimaltall mellom 0 og 1. Men hvis vi trenger et heltall, kan vi bruke Math.floor() for å runde av tallet nedover. La oss se på et eksempel:

```javascript
// Først genererer vi et tilfeldig desimaltall mellom 0 og 1
let randomDecimal = Math.random();
console.log(randomDecimal);

// Deretter bruker vi Math.floor() for å få et tilfeldig heltall mellom 0 og 10
let randomInt = Math.floor(Math.random() * 11);
console.log(randomInt);

// Vi kan også generere et tilfeldig heltall mellom et annet intervall, som for eksempel 50 og 100
let randomRange = Math.floor(Math.random() * (100 - 50 + 1)) + 50;
console.log(randomRange);
```

Dette er bare noen eksempler på hvordan vi kan generere tilfeldige tall ved hjelp av Javascript. Det finnes også andre metoder og funksjoner som kan brukes, avhengig av hvilke type tall du ønsker å generere.

## Deep Dive

Nå har vi sett på hvordan vi kan generere tilfeldige tall, men hvis du vil lære mer om tilfeldighet og tilfeldige tall i Javascript, er det flere ting du kan dykke dypere inn i. For eksempel kan du se på hvordan tilfeldighet spiller en rolle i algoritmer som skal velge ut tilfeldige elementer fra en liste. Du kan også undersøke hvordan tilfeldige tall kan brukes til å simulere ulike situasjoner og hvordan dette kan hjelpe i beslutningsprosesser.

Ved å dykke dypere inn i temaet kan du få en bedre forståelse av hvordan tilfeldighet fungerer og hvordan du kan utnytte det til å forbedre dine programmeringsferdigheter.

## Se også

- [Mozillas dokumentasjon om Math.random()](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [En artikkel om tilfeldige tall i dataverden](https://medium.com/@seanadiputra/random-numbers-in-computer-world-4bcc4a2a3615)
- [En veiledning for å forstå tilfeldighet i programmering](http://caternuson.github.io/2016/03/09/randomness-and-probability-for-programmers/)