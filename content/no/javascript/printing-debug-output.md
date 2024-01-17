---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Javascript: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Printing av debug output er en måte for programmerere å få mer informasjon om hvordan koden deres fungerer mens den kjører. Det kan gi programførere en bedre forståelse av hva som skjer under kjøring og bidra til å finne og fikse feil.

## Hvordan:

Det er flere måter å printe debug output i JavaScript, avhengig av hva du ønsker å se. Her er to vanlige tilnærminger som bruker `console.log()` funksjonen:

```Javascript
// Print en enkelt verdi
let num = 42;
console.log(num); // Output: 42

// Print flere verdier
let name = "Alice";
console.log("Hei, mitt navn er", name); // Output: Hei, mitt navn er Alice
```

## Fordypning:

Print debugging har vært en viktig del av programmering siden de tidligste dagene av datamaskiner. Men med fremveksten av moderne debugging-verktøy har det blitt mindre vanlig å bruke print debugging. Andre metoder som breakpoints og stack tracing er nå mer populære for å finne og fikse feil.

En annen viktig teknikk for å forbedre debugging er å bruke logger som registrerer en rekke hendelser eller verdier mens koden kjører. Dette kan gi en mer detaljert oversikt over hva som skjer i programmet.

Implementeringen av print debugging er enkel og krever ingen spesiell konfigurasjon. Det eneste som trengs er å bruke `console.log()` funksjonen og passende verdier eller uttrykk som skal printes.

## Se Også:

- [Console API dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Debugging JavaScript med Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/javascript/)