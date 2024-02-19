---
aliases:
- /no/kotlin/starting-a-new-project/
date: 2024-01-20 18:04:08.160687-07:00
description: "\xC5 starte et nytt prosjekt inneb\xE6rer \xE5 opprette en grunnleggende\
  \ ramme for kode du skal skrive. Programmerere gj\xF8r dette for \xE5 organisere\
  \ og strukturere\u2026"
lastmod: 2024-02-18 23:08:53.853022
model: gpt-4-1106-preview
summary: "\xC5 starte et nytt prosjekt inneb\xE6rer \xE5 opprette en grunnleggende\
  \ ramme for kode du skal skrive. Programmerere gj\xF8r dette for \xE5 organisere\
  \ og strukturere\u2026"
title: "\xC5 starte et nytt prosjekt"
---

{{< edit_this_page >}}

## What & Why?
Å starte et nytt prosjekt innebærer å opprette en grunnleggende ramme for kode du skal skrive. Programmerere gjør dette for å organisere og strukturere arbeidet sitt fra starten, noe som gjør det enklere å utvide og vedlikeholde koden.

## How to:
For å sette i gang et nytt Kotlin-prosjekt, kan du bruke IntelliJ IDEA eller et annet verktøy som støtter Kotlin. Her er de grunnleggende stegene:

```Kotlin
// Anta at IntelliJ IDEA er installert.
// 1. Åpne IntelliJ IDEA og velg "Create New Project".
// 2. Velg Kotlin i menyen av prosjekttyper.
// 3. Sett opp prosjektet med ønsket navn og plassering.
// 4. Start kodingen! For eksempel, en enkel "Hello, World!" app:
fun main() {
    println("Hei, Verden!")
}
```

Når du kjører koden, vil utdata være:

```
Hei, Verden!
```

## Deep Dive
Kotlin-prosjekter ble enklere å starte etter at JetBrains lanserte IntelliJ IDEA med innebygget støtte for Kotlin. Før det, måtte utviklere sette opp alt manuelt. Du har også flere alternativer, som kommandolinje-verktøyet `kotlinc` for å kompilere filer, eller å bruke Gradle eller Maven for prosjektbygging.

Nærmere informasjon:
- Historisk kontekst: Kotlin ble offisielt lansert av JetBrains i 2011 og nådde versjon 1.0 i 2016. Det er satt til å være et mer moderne alternativ til Java, spesielt for Android-utvikling.
- Alternativer: Utover IntelliJ IDEA, kan du bruke Eclipse eller Android Studio for Kotlin-utvikling.
- Implementasjonsdetaljer: Grunnleggende Kotlin-prosjekter krever en `main` funksjon som inngangspunkt. For mer avanserte prosjekter, vil du kanskje inkludere eksterne biblioteker, definere moduler, og sette opp byggeskript.

## See Also
- Offisiell Kotlin hjemmeside: [https://kotlinlang.org/](https://kotlinlang.org/)
- Kotlin dokumentasjon for å lære mer om språket: [https://kotlinlang.org/docs/reference/](https://kotlinlang.org/docs/reference/)
- GitHub repo med Kotlin-prosjekter for inspirasjon: [https://github.com/JetBrains/kotlin](https://github.com/JetBrains/kotlin)
