---
title:                "Skriving til standardfeil"
html_title:           "Javascript: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
For de fleste er standard error en ukjent og sjelden brukt funksjon i Javascript. Men det kan være en nyttig måte å feilsøke og forbedre koden din på. I denne artikkelen vil vi se nærmere på hvorfor det kan være lurt å skrive til standard error og hvordan man gjør det.

## Slik gjør du det
For å skrive til standard error, kan du bruke console.error() funksjonen i Javascript. Denne funksjonen tar imot en eller flere variabler som input, og skriver de ut i standard error konsollen. Her er et eksempel på hvordan du kan bruke funksjonen:

```Javascript
console.error("Dette er en feilmelding");
```

Dette vil resultere i at teksten "Dette er en feilmelding" blir skrevet ut i standard error konsollen.

Du kan også bruke interpolering for å vise variabler i feilmeldingen. Dette gjøres ved å bruke ${variabelnavn} inne i en tekststreng som vist nedenfor:

```Javascript
let num = 7;
console.error(`Variabelen num har verdien ${num}`);
```

Dette vil skrive ut teksten "Variabelen num har verdien 7" i standard error konsollen.

## Dypdykk
Nå lurer du sikkert på hvorfor dette er nyttig? En av fordelene med å skrive til standard error er at du kan skrive ut til konsollen selv når koden din ikke kjører. Dette kan være nyttig når du prøver å finne ut hvor koden din feiler. I tillegg vil feilmeldingene dine bli tydeligere og enklere å forstå for andre utviklere som skal bruke koden din.

Det er også viktig å merke seg at standard error er forskjellig fra standard output som er det som vanligvis blir skrevet til konsollen med console.log(). Mens standard output er ment for generell informasjon og debugging, er standard error ment for å indikere en feil i koden. Derfor vil det være mer hensiktsmessig å bruke console.error() for feilmeldinger i stedet for console.log().

## Se også
- [Hvordan bruke console.log() i Javascript](https://www.digitalocean.com/community/tutorials/how-to-use-the-javascript-console-log-function)
- [Hvordan feilsøke ved hjelp av console.error() i Javascript](https://blog.sessionstack.com/how-to-debug-javascript-errors-like-a-pro/)
- [Offisiell dokumentasjon for console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)