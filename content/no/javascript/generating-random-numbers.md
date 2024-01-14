---
title:                "Javascript: Dannelse av tilfeldige tall"
simple_title:         "Dannelse av tilfeldige tall"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av mange programmeringsprosjekter. Det kan være nyttig for å skape variasjon i spill og simuleringer, lage unike brukernavn eller passord, og teste algoritmer og funksjoner som trenger tilfeldig input.

## Slik gjør du det
For å generere tilfeldige tall i Javascript kan vi bruke Math.random() funksjonen. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1. Vi kan bruke dette til å generere tall i et gitt område ved å multiplisere og legge til andre tall. For eksempel, hvis vi vil ha et tilfeldig tall mellom 1 og 10, kan vi bruke følgende kode:

```Javascript
let min = 1;
let max = 10;
let randomNum = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNum);
```

Dette vil gi en utdata på et tilfeldig tall mellom 1 og 10, inkludert 1 og 10.

Vi kan også generere tilfeldige tegn ved å bruke ASCII-koder. For eksempel, hvis vi vil generere et tilfeldig bokstav fra a til z, kan vi bruke følgende kode:

```Javascript
let randomChar = String.fromCharCode(Math.floor(Math.random() * (122 - 97 + 1)) + 97);
console.log(randomChar);
```

Dette vil gi en tilfeldig bokstav fra a til z.

## Dypdykk
Det er viktig å merke seg at Math.random() ikke genererer ekte tilfeldige tall, men heller tall basert på en algoritme. Derfor kan det være lurt å bruke andre metoder for å generere tilfeldige tall hvis sikkerhet er et viktig faktor for programmet ditt.

En annen viktig ting å merke seg er at når du genererer tilfeldige tall i en løkke, bør du ikke kalle Math.random() hver gang. Dette er fordi funksjonen kan generere samme tall flere ganger på rad, noe som ikke er ønskelig. I stedet bør du kalle Math.random() utenfor løkken og lagre resultatet i en variabel som du bruker inni løkken.

## Se også
- [MDN dokumentasjon om Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Generering av tilfeldige tall i Java](https://michael-jamesscott.com/blog/How-To-Generate-Random-Numbers-In-Java/)
- [Security implications of using Math.random() for generating passwords](https://security.stackexchange.com/questions/219501/why-is-math-random-insecure-for-generating-passwords-and-tokens)