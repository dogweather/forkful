---
title:                "Generering av tilfeldige tall"
html_title:           "TypeScript: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å generere tilfeldige tall? Vel, det kan være en viktig del av mange programmeringsoppgaver og kan bidra til å gjøre koden mer dynamisk og variert.

## Slik gjør du det
Generering av tilfeldige tall i TypeScript er enkelt. Alt du trenger å gjøre er å bruke Math.random() metoden. La oss se på et eksempel:

```TypeScript
// Genererer et tilfeldig tall mellom 1 og 10
let randomNumber = Math.floor(Math.random() * 10) + 1; 

console.log(randomNumber); // Vil skrive ut et tilfeldig tall mellom 1 og 10 hver gang du kjører koden
```

Med dette eksempelet kan du generere et tilfeldig tall mellom et gitt område. Math.random() metoden vil alltid returnere et tall mellom 0 og 1, så ved å multiplisere det med det maksimale tallet du ønsker og legge til minimumumstallet, kan du få det ønskede resultatet.

Du kan også bruke denne metoden til å lage tilfeldige indekser for å hente elementer fra en array. La oss se på et annet eksempel:

```TypeScript
let myArray = [5, 10, 15, 20];
let randomIndex = Math.floor(Math.random() * myArray.length);

console.log(myArray[randomIndex]); // Vil skrive ut et tilfeldig tall fra arrayen hver gang du kjører koden
```

Som du kan se, kan generering av tilfeldige tall være nyttig i forskjellige situasjoner, og det er en enkel måte å legge til variasjon i koden din.

## Dykk dypere
Hvis du er interessert i å lære mer om tilfeldige tallgenerering i TypeScript, er det noen viktige ting du bør vite.

For det første, når du bruker Math.random() metoden, må du være oppmerksom på at den ikke genererer virkelig tilfeldige tall. Den bruker en algoritme som returnerer tall basert på en startverdi, kalt en seed, som vanligvis er systemsklokken. Seedet vil bestemme den første tilfeldige verdien som blir generert, og deretter vil hver påfølgende verdi bli bestemt av den forrige. Dette betyr at hvis du bruker samme seed, vil du få den samme sekvensen av tilfeldige tall hver gang.

En annen ting å være oppmerksom på er at Math.random() metoden ikke genererer et desimaltall nøyaktig. Den kan returnere et tall som ikke har en presisjon på flere desimaler. For å unngå denne ulempen, kan du multiplisere resultatet med et stort tall, som 1000, og deretter skjære av desimalene ved hjelp av Math.floor() metoden.

## Se også
- [TypeScript dokumentasjon om Math.random()](https://www.typescriptlang.org/docs/handbook/functions.html#the-importance-of-using-the---declaration)
- [Generering av tilfeldige tall i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)