---
title:                "Javascript: Søke og erstatte tekst"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Hvorfor

Det kan være mange grunner til å ønske å søke og erstatte tekst i Javascript. Kanskje du vil endre alle forekomster av et bestemt ord til et annet, eller kanskje du ønsker å formatere tekst på en spesiell måte. Uansett hva grunnen er, kan søking og erstatning av tekst være en nyttig ferdighet å ha når du jobber med Javascript.

##Slik gjør du det

For å søke og erstatte tekst i Javascript, kan du bruke metoden `replace` på en streng. Denne metoden tar to argumenter: den første er hva du ønsker å søke etter, og den andre er hva du ønsker å erstatte det med. La oss se på et eksempel:

```Javascript
let tekst = "Hei verden!";
let endretTekst = tekst.replace("Hei", "Hallo");
console.log(endretTekst);
```

I dette eksempelet bruker vi `replace`-metoden for å erstatte "Hei" med "Hallo" i strengen tekst. Outputen vil bli "Hallo verden!".

Det er også mulig å bruke regulære uttrykk i `replace`-metoden for å gjøre mer avanserte søk og erstatninger. Et regulært uttrykk er en måte å beskrive et mønster som en streng kan matche. La oss se på et eksempel der vi erstatter alle tall i en streng med "X":

```Javascript
let tall = "123456";
let endretTall = tall.replace(/[0-9]/g, "X");
console.log(endretTall);
```

I dette eksempelet bruker vi `[0-9]` for å matche alle tall i strengen ved å bruke et regulært uttrykk. Det siste argumentet `g` betyr global, som betyr at alle forekomster av mønsteret skal bli erstattet. Outputen vil bli "XXXXXX".

##Dypdykk

Å bruke regulære uttrykk i `replace`-metoden kan være veldig nyttig, spesielt når du ønsker å gjøre mer avanserte søk og erstatninger. Det finnes mange forskjellige mønstre du kan bruke i regulære uttrykk, og det kan være lurt å bruke et nettsted som regex101.com for å teste ut og utvikle dine egne uttrykk.

Det kan også være nyttig å vite at `replace`-metoden ikke endrer originalstrengen, den returnerer en ny streng med de nødvendige endringene. Derfor må du lagre resultatet i en ny variabel, som vi gjorde i eksemplene våre.

##Se også

- [MDN Web Docs - replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [regex101.com](https://regex101.com/)