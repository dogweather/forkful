---
title:    "Javascript: Skriving til standardfeil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error, eller stderr, er en nyttig måte å feilsøke og feilrapportere i JavaScript programmering. Ved å sende meldinger til stderr, kan du raskt identifisere og håndtere potensielle problemer i koden din.

## Slik gjør du det

For å skrive til standard error i JavaScript, kan du bruke funksjonen `console.error()`. Denne funksjonen tar inn en parameter, som kan være alt fra en enkel tekststreng til et objekt eller en variabel du ønsker å feilrapportere. Her er et eksempel:

```Javascript
let navn = "Ola";
console.error("Hei, mitt navn er", navn);
```

Når du kjører dette kodesnippet, vil du se at meldingen blir skrevet ut til stderr i konsollen. Output vil se omtrent slik ut:

```
Hei, mitt navn er Ola
```

Du kan også kombinere flere verdier sammen ved å bruke en strengsammenslåingsoperatør, for eksempel ved hjelp av `+`-tegnet:

```Javascript
console.error("Det er", 5 + 3, "elementer i denne listen");
```

Dette vil gi følgende output:

```
Det er 8 elementer i denne listen
```

## Dykk dypere

I tillegg til å bruke `console.error()`-funksjonen, kan du også skrive til stderr ved å bruke `process.stderr.write()`-metoden. Denne metoden tar også inn en parameter som kan være en tekststreng eller et objekt.

En annen viktig ting å merke seg er at stderr vanligvis er prioritert i forhold til standard output (stdout). Dette betyr at meldingene dine på stderr vil bli vist før stdout-meldingene. Dette kan være nyttig når du trenger å få rask tilbakemelding på eventuelle feil i koden din.

## Se også

- [Node.js dokumentasjon for console-modulen](https://nodejs.org/api/console.html)
- [Tutorial om å skrive til standard error i JavaScript](https://www.digitalocean.com/community/tutorials/how-to-use-the-javascript-console)
- [Artikkel om ulike metoder for å feilsøke i JavaScript](https://www.freecodecamp.org/news/how-to-debug-javascript/)