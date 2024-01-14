---
title:    "Kotlin: Lese en tekstfil"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig oppgave i mange programmeringsspråk, inkludert Kotlin. Dette er nyttig når du trenger å behandle informasjon lagret i en fil, som for eksempel brukerdata eller tekstinnhold. Å kunne lese en tekstfil åpner opp for mange muligheter for utviklere, og derfor er det viktig å forstå hvordan man gjør det i Kotlin.

## Hvordan gjøre det

For å lese en tekstfil i Kotlin, kan man bruke funksjonen `readLines()` som tar inn en filbane som parameter. Her er et eksempel på hvordan dette kan gjøres:

```Kotlin
val fil = File("brukerdata.txt")
val linjer = fil.readLines()
for(linje in linjer) {
    println(linje)
}
```

Koden ovenfor vil først opprette en `File`-objekt med navnet på tekstfilen du ønsker å lese. Deretter brukes `readLines()`-funksjonen for å lese innholdet av filen og lagre det i en liste av strenger. Til slutt brukes en `for`-løkke for å skrive ut hver linje i filen.

Kjørt med følgende innhold i tekstfilen:

```
Navn: Alex
Alder: 30
Bosted: Oslo
```

Vil følgende output vises i konsollen:

```
Navn: Alex
Alder: 30
Bosted: Oslo
```

## Dypdykk

For å forstå hvordan funksjonen `readLines()` fungerer, kan vi ta en titt på dokumentasjonen til Kotlin. Deretter kan vi se at denne funksjonen er en del av `File`-klassen, og at den bruker en metode som heter `readLines()` fra Java-biblioteket. Dette betyr at Kotlin bruker et eksisterende Java-basert kodebibliotek for å lese tekstfiler.

Det kan også være lurt å være oppmerksom på at ved å bruke `readLines()` vil hele innholdet av tekstfilen lastes inn i minnet før det blir behandlet. Dette kan være problematisk dersom filen er veldig stor, da det kan føre til at programmet går tom for minne. I så fall kan det være bedre å bruke en annen metode for å lese filen line for line istedenfor å laste alt inn på en gang.

## Se også

- [Kotlin Docs: File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java Docs: BufferedReader](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/BufferedReader.html)