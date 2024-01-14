---
title:                "TypeScript: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor generere tilfeldige tall?

Vi har alle vært der - du trenger et tilfeldig tall i et program, enten det er for å lage et spill, gjennomføre en vitenskapelig eksperiment eller bare for å teste noe ut. Men, hvordan kan du få tilgang til tilfeldige tall i et TypeScript-program? I denne blogginnlegget, vil jeg vise deg hvorfor det er viktig å bruke tilfeldige tall, hvordan du kan implementere det i TypeScript, og dykke dypere inn i hvordan tilfeldige tall egentlig fungerer.

## Slik genererer du tilfeldige tall i TypeScript

For å generere tilfeldige tall, må du først importere "Math" biblioteket i TypeScript. Deretter kan du bruke "random" funksjonen til å generere et tilfeldig tall mellom 0 og 1. For å få et tall med et større område, kan du multiplisere resultatet med ditt ønskede maksimumsverdi og legge til minimumsverdien for å justere området.

```TypeScript
import Math;

let tilfeldigTall = Math.random(); // Genererer et tall mellom 0 og 1
let tilfeldigStortTall = Math.random() * 100; // Genererer et tall mellom 0 og 100
let tilfeldigStortTallMedRange = Math.random() * (max - min) + min; // Genererer et tall mellom min og max
```

Output:

```
tilfeldigTall = 0.432
tilfeldigStortTall = 78.943
tilfeldigStortTallMedRange = 37.621 (hvis min = 20 og max = 55)
```

Du kan også få tilgang til tilfeldige tall basert på en bestemt liste ved å bruke "floor" funksjonen. Dette er nyttig hvis du for eksempel vil generere et tilfeldig navn fra en liste.

```TypeScript
let navn = ["Per", "Kari", "Ole", "Mia", "Jonas"];
let tilfeldigNavn = navn[Math.floor(Math.random() * navn.length)];

console.log(tilfeldigNavn); // Kan være enten "Per", "Kari", "Ole", "Mia" eller "Jonas"
```

Output:

```
Mia (tilfeldig valgt navn)
```

## Dykk dypere inn i tilfeldige tall

Men hvordan fungerer egentlig "Math.random" funksjonen? Den bruker faktisk et pseudorandom nummergenereringsalgoritme for å generere et tall. Dette betyr at algoritmen bruker matematiske beregninger for å produsere en sekvens av tall som ser ut til å være tilfeldige.

Men fordi algoritmen følger et bestemt mønster, kan det ikke generere virkelig tilfeldige tall. Dette er grunnen til at det kalles "pseudo" tilfeldige tall. Men det anses fremdeles som tilstrekkelig tilfeldig for mange anvendelser.

## Se også

1. [Generering av tilfeldige tall i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
2. [Pseudorandom tallgenerering for nybegynnere](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
3. [Hvordan generere spesifikke tilfeldige tall i TypeScript](https://stackoverflow.com/questions/34087498/how-to-generate-a-random-number-between-two-values-in-typescript)

Takk for at du leste denne guiden om hvordan du genererer tilfeldige tall i TypeScript. Forhåpentligvis har den vært nyttig for deg i fremtidige programmeringsprosjekter!