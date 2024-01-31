---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:15:46.852757-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En REPL (Read-Eval-Print Loop) er et enkelt, interaktivt programmeringsmiljø. Programmerere bruker det for raske kodingstester, teste kodebiter eller lære en programmeringsspråks syntaks uten å lage en fullstendig applikasjon.

## Hvordan:
Å starte Kotlin sin REPL er en lek. Åpne terminalen din og skriv `kotlinc`. Du vil havne i Kotlin-skallet. La oss prøve å definere en variabel og skrive ut verdien dens:

```kotlin
Velkommen til Kotlin versjon 1.7.10 (JRE 1.8.0_292-b10)
Skriv :help for hjelp, :quit for å avslutte
>>> val hilsen = "Hallo, Kotlin REPL!"
>>> println(hilsen)
Hallo, Kotlin REPL!
```

## Dypdykk
Kotlin sin REPL debuterte med språket for å oppmuntre til eksperimentering. Det er likt Python sitt interaktive skall, men tilpasset Kotlin sin syntaks og særegenheter. Alternativer? Interaktive miljøer i IDEer, slik som IntelliJ IDEA, og nettbaserte Kotlin-lekeplasser. REPL fungerer ved å kompilere kode på flueben, noe som gir øyeblikkelig tilbakemelding – avgjørende for læring og feilsøking.

## Se også
- Kotlin dokumentasjon om REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Prøv Kotlin i nettleseren: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- JetBrains Kotlin Playground-plugin for IntelliJ IDEA.
