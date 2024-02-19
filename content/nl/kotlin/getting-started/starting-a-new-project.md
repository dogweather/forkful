---
aliases:
- /nl/kotlin/starting-a-new-project/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:55.904030-07:00
description: "Een nieuw project starten betekent het opzetten van de initi\xEBle structuur\
  \ en essenti\xEBle bestanden die je nodig hebt voor je applicatie. Programmeurs\u2026"
lastmod: 2024-02-18 23:09:01.806327
model: gpt-4-0125-preview
summary: "Een nieuw project starten betekent het opzetten van de initi\xEBle structuur\
  \ en essenti\xEBle bestanden die je nodig hebt voor je applicatie. Programmeurs\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten betekent het opzetten van de initiële structuur en essentiële bestanden die je nodig hebt voor je applicatie. Programmeurs initiëren nieuwe projecten om de ontwikkeling te starten met een schone lei, afgestemd op hun doelen en de technologieën die ze van plan zijn te gebruiken.

## Hoe:

Laten we beginnen met een Kotlin project met IntelliJ IDEA - een populaire IDE voor Kotlin.

1. Open IntelliJ IDEA.
2. Selecteer `Bestand > Nieuw > Project`.
3. Kies `Kotlin` in de linkerbalk.
4. Specificeer de SDK van je project (gewoonlijk selecteert IntelliJ de meest recente die je hebt geïnstalleerd).
5. Kies een projectsjabloon of houd de standaardsjabloon.
6. Geef je project een naam en kies de locatie ervan.
7. Klik op `Voltooien`.

Bam, je hebt een nieuw Kotlin project. Je typische directory zal er na creatie zo uitzien:

```plaintext
projectNaam
|-- .idea
|-- src
     |-- main.kt
|-- build.gradle
```

En je `main.kt` zou kunnen beginnen zo simpel als dit:

```kotlin
fun main() {
    println("Klaar, voor de start, ga Kotlin!")
}
```

Bij het uitvoeren van de `main.kt`, zul je zien:

```plaintext
Klaar, voor de start, ga Kotlin!
```

## Diepere duik

Kotlin is een moderne taal die draait op de JVM (Java Virtual Machine), ontworpen om beknopt en veilig te zijn. Het is gecreëerd door JetBrains en heeft vooral voor de ontwikkeling van Android-apps aan trek gewonnen, sinds Google in 2017 officiële ondersteuning aankondigde.

Voordat je begint met een nieuw Kotlin project, begrijp waarom je voor Kotlin kiest:
- Beknopte syntaxis: Vermindert boilerplate code.
- Interoperabel met Java: Naadloze integratie met Java-code en bibliotheken.
- Slimme casting: Minder expliciete typecasting nodig.
- Null-veiligheid: Ingebouwd systeem om null pointer exceptions te voorkomen.

Alternatieven voor het starten van een nieuw Kotlin-project met IntelliJ IDEA:
- Command-Line: Maak bestanden handmatig aan, compileer met `kotlinc` en voer uit met `kotlin` commando's.
- Andere IDE's: Gebruik Android Studio voor Android-ontwikkeling of Eclipse met de Kotlin-plugin.

Wanneer je een nieuw project start in IntelliJ IDEA, stelt het automatisch de benodigde Gradle-configuratie in. Gradle is een systeem voor bouwautomatisering dat afhankelijkheden, builds en tests voor je project beheert. Deze opzet stelt je in staat om bibliotheken te importeren, moduleafhankelijkheden te definiëren en builds met gemak te faciliteren.

## Zie ook

Wil je verder gaan dan de basis? Hier kun je vervolgens terecht:

- Officiële Kotlin documentatie: [Kotlin Docs](https://kotlinlang.org/docs/home.html)
- Basisprincipes van Gradle: [Gradle-gebruikershandleiding](https://docs.gradle.org/current/userguide/userguide.html)

Onthoud, de beste manier om te leren is door te doen. Begin met de 'Hello World' en bouw daar vanaf verder. Veel plezier met coderen!
