---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er prosessen med å produsere tallsekvenser som ikke kan forutsies på forhånd. Dette er nyttig for slike oppgaver som spill, simuleringer, og sikkerhetsapplikasjoner som krever unikke nøkler.

## Hvordan:

Her er en enkel måte å generere et tilfeldig tall mellom 0 (inkludert) og 1 (ekskludert) i TypeScript:

```TypeScript
let random = Math.random();
console.log(random);
```

Hvis du vil ha et tilfeldig tall mellom min (inkludert) og max (ekskludert), kan du bruke følgende funksjon:

```TypeScript
function getRandomArbitrary(min: number, max: number) {
    return Math.random() * (max - min) + min;
}
console.log(getRandomArbitrary(1,100));
```

## Dyp Dykk

1. Historisk kontekst: "Pseudorandom" nummergeneratorer har blitt brukt i databehandling siden den tidlige dagen av programmering. Disse inkluderer metoder som midtpunktskvadratsmetoden, lineær kongruential generator, og Mersennes tvillinggenerator.

2. Alternativer: For sikkerhetskritiske applikasjoner kan kryptografisk sikre pseudotilfeldige tallgeneratorer (CSPRNGs) som /dev/random i Unix systemer, eller window.crypto.getRandomValues i nettleseren være nyttige.

3. Implementeringsdetaljer: Math.random() i JavaScript (og derfor TypeScript) bruker normalt en variant av Mersennes tvillinggenerator.

## Se Også

1. [The Art of Computer Programming: Volume 2](https://www.amazon.com/books/dp/8120315221) - Bestselgeren av Donald E. Knuth dekker mange forskjellige aspekter, inkludert tilfeldige tall.

2. [Mozilla's Math.random() dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) gir mer innsikt i hvordan denne funksjonen fungerer.

3. [PCG, A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation](http://www.pcg-random.org/) gir en introduksjon og forklaring på en moderne tilfeldig tallgenerator.