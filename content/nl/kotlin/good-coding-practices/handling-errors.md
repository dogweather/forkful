---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:58.414702-07:00
description: "Het afhandelen van fouten is hoe je code omgaat met problemen die opduiken\
  \ tijdens de uitvoering - zoals een onverwachte wending zonder deze te laten\u2026"
lastmod: '2024-03-13T22:44:50.775367-06:00'
model: gpt-4-0125-preview
summary: "Het afhandelen van fouten is hoe je code omgaat met problemen die opduiken\
  \ tijdens de uitvoering - zoals een onverwachte wending zonder deze te laten\u2026"
title: Fouten afhandelen
weight: 16
---

## Wat & Waarom?
Het afhandelen van fouten is hoe je code omgaat met problemen die opduiken tijdens de uitvoering - zoals een onverwachte wending zonder deze te laten vallen. Programmeurs doen dit om crashes te voorkomen en gebruikers een soepele ervaring te geven.

## Hoe:
Kotlin biedt `try`, `catch`, `finally`, en `throw` om fouten te beheren. Hier is hoe je ze gebruikt:

```Kotlin
fun main() {
    val teller = 10
    val noemer = 0

    try {
        val resultaat = teller / noemer
        println("Resultaat: $resultaat")
    } catch (e: ArithmeticException) {
        println("Je kunt niet delen door nul, maat.")
    } finally {
        println("Dit gebeurt hoe dan ook.")
    }
}
```

Output:
```
Je kunt niet delen door nul, maat.
Dit gebeurt hoe dan ook.
```

Als er iets fout gaat in het `try` blok, springt de uitvoering naar `catch`. Het vangt de specifieke fout op die gegooid wordt (`ArithmeticException` in dit geval). Het `finally` blok voert daarna uit—ongeacht de uitkomst.

## Diepgaand
Het `try-catch` blok bestaat al sinds de vroege programmeerdagen—het is als een veiligheidsnet. Kotlin biedt ook `throw` aan om handmatig een uitzondering in de ring te gooien, en er is `finally` voor code die moet draaien—vaak opruimwerk.

Alternatieven zijn het `Result` type en Kotlin's `try` als een expressie.

```Kotlin
val resultaat: Result<Int> = try {
    Result.success(teller / noemer)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Deze aanpak retourneert een `Result` object—je krijgt ofwel een succes of een mislukking zonder het drama van een onbehandelde uitzondering.

Implementatie in Kotlin is netjes omdat je `try` kunt gebruiken als een expressie, wat betekent dat het een waarde retourneert. Keuzes zoals deze maken de foutafhandeling in Kotlin vrij veelzijdig. Het gaat erom het juiste gereedschap voor de klus te kiezen, net zoals je dat in een werkplaats zou doen.

## Zie Ook
- Kotlin documentatie over Uitzonderingen: [Kotlin Exception Handling](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin `Result` type documentatie: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3e Editie, door Joshua Bloch—geweldige inzichten over uitzonderingen, hoewel het specifiek over Java gaat.
