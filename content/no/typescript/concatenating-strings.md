---
title:    "TypeScript: Sammenføyning av strenger"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, også kjent som å "knekke" dem, er en viktig del av TypeScript-programmering. Det lar deg legge sammen to eller flere strenger for å lage en ny streng. Dette er spesielt nyttig når du ønsker å lage en dynamisk streng som inkluderer variabler eller data fra brukeren.

## Hvordan du gjør det

Det er flere måter å kombinere strenger på i TypeScript. Den mest grunnleggende metoden er å bruke plussoperatøren (+). Her er et eksempel på hvordan du kan kombinere to strenger:

```TypeScript
let navn = "Ole";
let hilsen = "Hei, " + navn + "!";
console.log(hilsen);
```

Output:

```
Hei, Ole!
```

Du kan også bruke += operator for å legge til en streng til en annen streng:

```TypeScript
let setning = "Jeg liker";
setning += " å programmere.";
console.log(setning);
```

Output:

```
Jeg liker å programmere.
```

Enda en måte å kombinere strenger på er å bruke string interpolation. Dette lar deg sette inn variabler direkte inn i en streng ved å bruke backtick tegn (`) og legge til variabelens navn inne i ${} som vist her:

```TypeScript
let tall1 = 5;
let tall2 = 10;
let sum = `Summen av ${tall1} og ${tall2} er ${tall1 + tall2}.`;
console.log(sum);
```

Output:

```
Summen av 5 og 10 er 15.
```

Det er også verdt å merke seg at du kan kombinere ikke-bare strenger, men også nummer og boolske verdier ved å bruke plussoperatøren.

## Dypdykk

Når du kombinerer strenger i TypeScript, er det viktig å huske at resultatet alltid vil være en streng. Det betyr at hvis du kombinerer en streng og et tall, vil tallet automatisk bli konvertert til en streng og deretter legges til den eksisterende strengen.

En annen viktig ting å huske er å bruke passende mellomrom når du kombinerer strenger. Hvis du ikke bruker mellomrom, vil resultatet bli en sammenhengende streng uten noen plass mellom ordene.

Det er også viktig å være klar over at mens du kan kombinere så mange strenger du vil, kan det påvirke ytelsen til programmet ditt hvis du kombinerer mange lange strenger. I slike tilfeller er det bedre å bruke en annen metode, for eksempel å bruke et array og deretter bruke join() funksjonen til å kombinere strengene.

## Se også

- [TypeScript offisiell dokumentasjon om strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [FreeCodeCamp tutorial om hvordan du bruker strings i TypeScript](https://www.freecodecamp.org/news/the-basics-of-string-interpolation-in-typescript-tutorial/)
- [YouTube tutorial om concatenating strings i TypeScript](https://www.youtube.com/watch?v=RAI5Jc6Swx0)