---
title:    "Kotlin: Skriver en tekstfil"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor
Skriver du ofte programmer i Kotlin og lurer på hvorfor du skulle bry deg med å skrive en tekstfil? Det kan virke som en unødvendig oppgave, men det er faktisk flere gode grunner til å lære hvordan det gjøres.

## Hvordan
Først må du importere nødvendige pakker og klasser:

```Kotlin
import java.io.File
import java.io.FileWriter
```

Deretter kan du enkelt opprette en fil og skrive tekst til den ved å bruke ```FileWriter```:

```Kotlin
val file = File("minTekstfil.txt")
file.writeText("Hei, dette er en tekstfil!")
```

For å lese innholdet i filen, kan du bruke ```readText()``` metoden:

```Kotlin
val innhold = file.readText()
println(innhold) // Utskrift: Hei, dette er en tekstfil!
```

Du kan også skrive til filen linje for linje ved å bruke en ```BufferedWriter```:

```Kotlin
val bufferedWriter = BufferedWriter(FileWriter("minTekstfil.txt"))
bufferedWriter.write("Dette er første linje.")
bufferedWriter.newLine()
bufferedWriter.write("Dette er andre linje.")
bufferedWriter.close()
```

## Dypdykk
Nå som du vet hvordan du kan opprette og skrive til en tekstfil, kan det være nyttig å vite litt mer om hvordan dette fungerer. En tekstfil er egentlig bare en samling av bytes, og det er viktig å være bevisst på hvilken tegnkode (charset) du bruker når du leser og skriver til filen. Vanligvis anbefales det å bruke UTF-8.

Det er også viktig å huske på å lukke filen etter bruk. Dette gjøres automatisk hvis du bruker en ```BufferedWriter```, men hvis du bruker ```writeText()``` metoden, må du huske å kalle ```close()``` metoden på filen for å unngå eventuelle problemer.

## Se også
- [Java-tutorial: Skrive til en fil](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Kotlins offisielle dokumentasjon om å skrive/løse innhold fra filer](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-writing-files.html)