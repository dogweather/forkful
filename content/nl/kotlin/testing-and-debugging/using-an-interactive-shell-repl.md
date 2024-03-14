---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:24.155386-07:00
description: "Een REPL (Read-Eval-Print Loop) is een eenvoudige, interactieve computerprogrammeeromgeving.\
  \ Programmeurs gebruiken het voor snelle codeerproeven, het\u2026"
lastmod: '2024-03-13T22:44:50.769495-06:00'
model: gpt-4-0125-preview
summary: "Een REPL (Read-Eval-Print Loop) is een eenvoudige, interactieve computerprogrammeeromgeving.\
  \ Programmeurs gebruiken het voor snelle codeerproeven, het\u2026"
title: Het gebruik van een interactieve shell (REPL)
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
