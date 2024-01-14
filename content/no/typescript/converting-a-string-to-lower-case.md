---
title:                "TypeScript: Omgjøring av streng til små bokstaver"
simple_title:         "Omgjøring av streng til små bokstaver"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger i programmering må vi jobbe med tekster, og noen ganger kan det være nødvendig å konvertere en tekst til små bokstaver. Dette kan være nyttig for søk og sammenligning i databaser, eller bare for å få en jevn formatering av teksten. Heldigvis har TypeScript innebygd funksjonalitet for å gjøre dette enkelt.

## Hvordan gjøre det
La oss starte med å definere en tekststreng i TypeScript:
```TypeScript
let tekst = "DETTE ER EN TEKST"
```
For å konvertere denne til små bokstaver, kan vi bruke TypeScript sin `toLowerCase()` metode:
```TypeScript
let tekst = "DETTE ER EN TEKST"
console.log(tekst.toLowerCase()) // output: dette er en tekst
```
Som du kan se, tar denne metoden vår originale tekst og konverterer den til små bokstaver.

Vi kan også bruke denne metoden direkte på en tekst uten å definere en variabel:
```TypeScript
console.log("JEG VIL BLI TIL SMÅ BOKSTAVER".toLowerCase()) // output: jeg vil bli til små bokstaver
```

## Dypdykk
Det er viktig å merke seg at denne metoden ikke bare konverterer store bokstaver til små, men den erkjenner også spesielle tegn og bokstavkombinasjoner. For eksempel vil en bokstav med aksent fortsatt være med i sin konverterte form.
```TypeScript
console.log("Håndbok".toLowerCase()) // output: håndbok
```
Dette er spesielt viktig å huske når du jobber med flerspråklige tekster.

Det er også verdt å nevne at hvis du vil beholde den originale tekststrengen og bare få en konvertert kopi, kan du bruke `toLowerCase()` metoden på en variabel og lagre resultatet i en ny variabel:
```TypeScript
let originalTekst = "NAVN"
let konvertertTekst = originalTekst.toLowerCase()
console.log(konvertertTekst) // output: navn
```

## Se også
* [TypeScript: String toLowerCase() metode](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-0.html#new-support-for-stringliteraltypes)