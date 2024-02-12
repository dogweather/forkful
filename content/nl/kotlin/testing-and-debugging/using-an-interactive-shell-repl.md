---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- /nl/kotlin/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:24.155386-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een REPL (Read-Eval-Print Loop) is een eenvoudige, interactieve computerprogrammeeromgeving. Programmeurs gebruiken het voor snelle codeerproeven, het testen van codefragmenten, of het leren van de syntax van een taal zonder een volledige applicatie te creëren.

## Hoe te:
Het starten van Kotlin's REPL is een fluitje van een cent. Open je terminal en typ `kotlinc`. Je belandt in de Kotlin-shell. Laten we proberen een variabele te definiëren en de waarde ervan te printen:

```kotlin
Welkom bij Kotlin versie 1.7.10 (JRE 1.8.0_292-b10)
Typ :help voor hulp, :quit om te stoppen
>>> val groet = "Hallo, Kotlin REPL!"
>>> println(groet)
Hallo, Kotlin REPL!
```

## Diepgaand
Kotlin's REPL is geïntroduceerd met de taal om experimentatie aan te moedigen. Het lijkt op Python's interactieve shell maar is aangepast voor Kotlin's syntax en eigenaardigheden. Alternatieven? Interactieve omgevingen in IDE's, zoals IntelliJ IDEA, en online Kotlin speeltuinen. De REPL werkt door code on-the-fly te compileren, wat directe feedback biedt – cruciaal voor leren en debuggen.

## Zie Ook
- Kotlin documentatie over REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Probeer Kotlin in de browser: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- JetBrains Kotlin Playground plugin voor IntelliJ IDEA.
