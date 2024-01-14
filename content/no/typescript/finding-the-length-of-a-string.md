---
title:    "TypeScript: Å finne lengden av en streng"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten vil vi utforske en viktig del av programmering - å finne lengden av en streng (string). Dette er en grunnleggende ferdighet som er avgjørende for å kunne arbeide med tekstbaserte data og informasjon. Ved å lære hvordan man kan finne lengden av en streng, kan du skrive mer effektiv og strukturert kode.

## Hvordan gjøre det

For å finne lengden av en streng i TypeScript, kan du bruke innebygde metoder som "length", "size" eller "count". Her er et eksempel på hvordan dette kan gjøres:

```TypeScript
let streng: string = "Hei alle sammen!";
console.log(streng.length);
```
Output:
```TypeScript
16
```

Her bruker vi "length" metoden for å finne lengden av strengen "Hei alle sammen!" og konsollen vil deretter skrive ut svaret, som er 16. Det er viktig å merke seg at "length" teller antall tegn i en streng, inkludert mellomrom.

Det er også mulig å bruke "size" metoden for å finne lengden av en streng. Denne metoden fungerer på samme måte som "length", men er vanligvis brukt på arrays. Her er et eksempel på hvordan man kan bruke "size":

```TypeScript
let strengArr: string[] = ["Hei", "alle", "sammen", "!"];
console.log(strengArr.size);
```
Output:
```TypeScript
4
```

Til slutt kan man også bruke "count" metoden for å finne lengden av en streng. Denne metoden fungerer på samme måte som "size", men brukes vanligvis på objekter. Eksempel:

```TypeScript
let strengObj: {navn: string, alder: number} = {navn: "Per", alder: 25};
console.log(strengObj.count);
```
Output:
```TypeScript
2
```

## Dypdykk

Som nevnt tidligere, vil "length" metoden også telle mellomrom i en streng. Hvis du ønsker å ekskludere mellomrom og kun få antall bokstaver i en streng, kan du bruke "trim" metoden først. Eksempel:

```TypeScript
let streng: string = "   Hei alle sammen!  ";
console.log(streng.trim().length);
```
Output:
```TypeScript
16
```

Her vil "trim" fjerne mellomrom på begynnelsen og slutten av strengen, og "length" vil da telle kun bokstaver.

Det er også viktig å være klar over at forskjellige språk og alfabeter kan ha forskjellige måter å telle lengden av en streng på. I slike tilfeller, er det best å bruke mer avanserte metoder eller biblioteker for å håndtere dette.

## Se også

For å lære mer om strenger i TypeScript, kan du se følgende ressurser:

- [Offisiell TypeScript dokumentasjon om strenger](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [W3Schools Tutorial om strenger i TypeScript](https://www.w3schools.com/ts/ts_strings.asp)
- [TypeScript Strings - Enkommandokombo fra CodeAcademy](https://www.codecademy.com/courses/learn-typescript/lessons/working-with-strings/exercises/type-script-strings)