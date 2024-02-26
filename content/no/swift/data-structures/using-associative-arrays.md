---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:30.968796-07:00
description: "Assosiative tabeller, kjent som ordb\xF8ker i Swift, lar deg lagre og\
  \ h\xE5ndtere data som n\xF8kkel-verdi-par. Programmerere bruker dem til \xE5 organisere\
  \ data\u2026"
lastmod: '2024-02-25T18:49:39.313888-07:00'
model: gpt-4-0125-preview
summary: "Assosiative tabeller, kjent som ordb\xF8ker i Swift, lar deg lagre og h\xE5\
  ndtere data som n\xF8kkel-verdi-par. Programmerere bruker dem til \xE5 organisere\
  \ data\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent som ordbøker i Swift, lar deg lagre og håndtere data som nøkkel-verdi-par. Programmerere bruker dem til å organisere data effektivt, noe som gjør det lettere å få tilgang til og manipulere verdier basert på deres unike nøkler.

## Hvordan:

Swift gjør det enkelt å jobbe med assosiative tabeller. Her er hvordan du kan erklære, legge til, fjerne og få tilgang til elementer i en Swift-ordbok:

```Swift
// Erklærer en ordbok
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Legger til et nytt element
fruitColors["Grape"] = "Purple"

// Får tilgang til en verdi ved hjelp av dens nøkkel
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // Utdata: Apple is Red.
} else {
    print("Farge ikke funnet.")
}

// Fjerner et element
fruitColors["Banana"] = nil  // Dette vil fjerne "Banana" fra ordboken

// Itererer over elementer
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // Utdata:
    // Apple is Red.
    // Grape is Purple.
}
```

Ordbøker er utrolig allsidige og lar deg manipulere og få tilgang til data dynamisk. Deres uordnede natur påvirker ikke hastigheten på datahenting, noe som er en vesentlig fordel ved håndtering av store datasett.

## Dypdykk

Swifts implementering av ordbøker som en assosiativ tabell stammer fra deres kraftige evne til å kartlegge unike nøkler til verdier. Historisk sett har programmeringsspråk implementert dette konseptet under forskjellige navn som hashtabeller eller kart, noe som antyder deres funksjonalitet til å lage et "kart" mellom nøkler og verdier.

I Swift er ordbøker optimalisert for ytelse ved å utnytte hashable nøkler for effektiv datahenting. Dette betyr at `Key`-typen i en `[Key: Value]`-ordbok må følge `Hashable`-protokollen, noe som er tilfellet for de fleste Swift-standarde typer som `Int`, `String`, og `Double`.

En ting å vurdere er at mens ordbøker er utmerkede for å assosiere par av data, mangler de rekkefølge. Hvis du trenger å opprettholde elementenes rekkefølge, kan du utforske alternativer som `Array` for en sekvens av ordnede elementer eller egendefinerte datastrukturer som kombinerer funksjonene til både tabeller og ordbøker.

Det er også verdt å merke seg at Swift er i kontinuerlig utvikling, og det samme gjelder håndtering og optimaliseringer av ordbøker. Derfor er det avgjørende å holde seg oppdatert med den nyeste Swift-dokumentasjonen for å utnytte det meste ut av ordbøker, og sørge for at du bruker de mest effektive og oppdaterte praksisene.
