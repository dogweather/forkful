---
title:                "Javascript: Skriving til standardfeil"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Skriving til standardfeil (standard error) er en viktig ferdighet for enhver JavaScript-programmer. Det lar deg effektivt fange og håndtere feil i koden din, noe som er avgjørende for å lage pålitelige og robuste programmer. Uten å skrive til standardfeil, kan feil og unntak gå ubemerket hen, noe som kan føre til store problemer i både utvikling og produksjon.

# Hvordan

For å skrive til standardfeil i JavaScript, kan du bruke funksjonen `console.error()`. Denne funksjonen tar inn et argument som skal skrives til standardfeil og sender det til konsollen. La oss se på et eksempel:

```Javascript
let num1 = 10;
let num2 = "5";
let result = num1 / num2;
console.error("Kan ikke dele string med tall. Resultatet er: ", result);
```

I dette tilfellet vil `console.error()` skrive ut følgende melding til standardfeil:

```
"Kan ikke dele string med tall. Resultatet er: NaN"
```

Dette er spesielt nyttig når du ønsker å fange feil og avbryte programmet ditt, slik at du kan feilsøke og rette opp eventuelle problemer.

# Dypdykk

Når du skriver til standardfeil, er det viktig å merke seg at meldingene blir sendt til konsollen og ikke til brukeren av programmet. Derfor bør meldingene være informative og hjelpe deg med å identifisere feilen.

En annen viktig ting å merke seg er at du også kan bruke `console.error()` til å håndtere unntak i try/catch-blokker. Dette gjør det enklere å fange og håndtere feil i koden din.

En annen nyttig funksjon for å skrive til standardfeil er `console.trace()`. Denne funksjonen skriver ut en stakksporing (stack trace) som viser kallstabelen for der feilen oppstod. Dette kan være svært nyttig når du jobber med kompleks kode og trenger å finne ut nøyaktig hvor et problem oppsto.

# Se også

Her er noen nyttige lenker for å lære mer om skriving til standardfeil i JavaScript:

- [MDN - console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [MDN - try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [MDN - console.trace()](https://developer.mozilla.org/en-US/docs/Web/API/Console/trace)

Med disse ressursene og kunnskapen du har nå, bør du være godt rustet til å skrive til standardfeil og håndtere feil i JavaScript-programmeringen din. Lykke til!