---
title:    "TypeScript: Utvinning av delstrenger"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å eksakte delstrenger fra en streng er en vanlig oppgave i programmering. Dette kan være nyttig for å behandle data eller manipulere tekster. Med TypeScript, et typet språk utviklet av Microsoft, kan dette gjøres på en enkel og effektiv måte. Les videre for å lære mer om hvordan man ekstrakterer delstrenger i TypeScript.

## Hvordan gjøre det

Først må man ha en streng å jobbe med. I TypeScript kan dette gjøres ved å definere en variabel med typen `string`. La oss bruke følgende som et eksempel: 
```TypeScript
let setning: string = "Det er en vakker dag i Norge";
```
For å ekstraktere en delstreng fra starten av denne teksten, kan man bruke `.substring()` metoden og angi start- og sluttpunktet til delstrengen man ønsker å hente ut. I dette tilfellet vil vi fjerne de første 11 bokstavene, som er "Det er en". Dette kan gjøres på følgende måte:
```TypeScript
let delstreng: string = setning.substring(11);
console.log(delstreng); // vil gi oss "vakker dag i Norge"
```
Man kan også angi en sluttposisjon som argument til `.substring()` metoden for å ekstraktere en delstreng mellom to gitt punkter. For eksempel, dersom vi ønsker å hente ut "vakker dag", kan vi skrive:
```TypeScript
let delstreng: string = setning.substring(11, 21);
console.log(delstreng); // vil gi oss "vakker dag"
```

## Dypdykk

Det er verdt å merke seg at når man bruker `.substring()` metoden, vil den returnere en ny streng og ikke endre den originale strengen. Dette er spesielt nyttig dersom man ønsker å jobbe med den samme strengen senere, uten å miste dens opprinnelige verdi. Det er også flere metoder som kan brukes for å ekstraktere delstrenger i TypeScript, som for eksempel `.slice()` og `.substr()`. Disse kan være nyttige i ulike situasjoner, så det kan være lurt å utforske dem også.

## Se også

* [TypeScript substring dokumentasjon](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
* [W3Schools tutorial om delstrenger i TypeScript](https://www.w3schools.com/jsref/jsref_substr.asp)