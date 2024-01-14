---
title:    "Javascript: Å bruke regulære uttrykk"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor bruker vi regulære uttrykk?

Regulære uttrykk, også kjent som regex, er et kraftig verktøy for å finne, filtrere og manipulere tekststrenger. Dette gjør det til et uunnværlig verktøy for webutviklere, dataanalytikere og mange andre som jobber med tekstbehandling i programmering. Ved å lære å bruke regulære uttrykk kan du enkelt automatisere oppgaver som ellers ville tatt mye tid og krefter å gjøre manuelt.

## Hvordan bruke regulære uttrykk

Det enkleste måten å matche et regulært uttrykk på er ved å bruke ```test()``` metoden. Dette vil returnere true eller false avhengig av om uttrykket matcher med den gitte teksten. For eksempel:

```Javascript
const uttrykk = /^[a-z]+[0-9]{2}$/; // Dette uttrykket vil matche en tekststreng som starter med en eller flere små bokstaver og avsluttes med to tall
const tekst = "abc123";

if (uttrykk.test(tekst)) {
    console.log("Uttrykket matcher med teksten");
} else {
    console.log("Uttrykket matcher ikke med teksten");
}
// Output: Uttrykket matcher med teksten
```

En annen måte å bruke regulære uttrykk på er ved å bruke ```exec()``` metoden. Dette returnerer et array med informasjon om matchen. For eksempel, hvis vi vil finne alle tall i en tekststreng, kan vi bruke følgende kode:

```Javascript
const uttrykk = /[0-9]+/g; // Dette uttrykket vil gi alle tall i en tekststreng
const tekst = "Hei, jeg er 25 år gammel.";

let resultat;
while ((resultat = uttrykk.exec(tekst)) !== null) {
    console.log("Fant tall: " + resultat[0]);
}
// Output: Fant tall: 25
```

## Dypdykk i regulære uttrykk

Regulære uttrykk har en lang liste med spesielle symboler og uttrykk som kan virke overveldende for nybegynnere. Det er viktig å forstå de grunnleggende konseptene og kunne lese og skrive enkle uttrykk før du begynner å dykke dypere inn i dette temaet. Det finnes også mange forskjellige online verktøy og guider som kan hjelpe deg med å forstå og beherske regulære uttrykk.

## Se også

- [MDN Web Docs: Regulære uttrykk](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101](https://regex101.com/) - Et online verktøy for å teste og eksperimentere med regulære uttrykk.
- [30 sekunders regex](https://github.com/30-seconds/30-seconds-of-code#regex) - En samling av korte og nyttige regex-eksempler.