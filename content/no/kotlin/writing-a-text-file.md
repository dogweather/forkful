---
title:                "Skriver en tekstfil"
html_title:           "Kotlin: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig oppgave i mange programmeringsspråk, og Kotlin er intet unntak. Å kunne skrive tekstfiler kan være nyttig for å lagre og behandle data i et program.

## Hvordan gjøre det

Skriving av en tekstfil i Kotlin er en enkel prosess som kan gjøres på få linjer med kode. Først må vi opprette en variabel som inneholder teksten vi vil skrive til filen:

```Kotlin
val tekst = "Dette er en tekstfil skrevet med Kotlin"
```

Deretter må vi bruke en FileWriter-klasse for å åpne en ny fil for skriving, og en BufferedWriter for å skrive teksten til filen:

```Kotlin
val skriver = BufferedWriter(FileWriter("tekstfil.txt"))
skriver.write(tekst)
```

Til slutt må vi lukke både skriveren og filen:

```Kotlin
skriver.close()
```

Etter å ha kjørt disse tre linjene med kode, vil teksten bli skrevet til filen "tekstfil.txt".

## Gå dypere

Hvis vi ønsker å legge til mer tekst i filen vår, kan vi bruke metoden "append()" i FileWriter-klassen:

```Kotlin
val skriver = BufferedWriter(FileWriter("tekstfil.txt", true))
skriver.append("Dette er en tilleggslinje")
```

Vi kan også bruke BufferedWriter til å skrive flere linjer med tekst ved å bruke metoden "newLine()". Dette vil legge til en ny linje i filen vår og gjøre det lettere å lese:

```Kotlin
skriver.newLine()
skriver.write("Dette er en ny linje")
```

## Se også

- [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
- [Writing Text Files in Java](https://www.baeldung.com/java-write-to-file)
- [Kotlin Tutorials on YouTube](https://www.youtube.com/watch?v=t5N_RtPSsta)