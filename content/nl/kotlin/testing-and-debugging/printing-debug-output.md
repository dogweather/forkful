---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:31.991461-07:00
description: "Debugoutput afdrukken is in wezen de manier waarop een programmeur een\
  \ kijkje neemt naar wat er in real-time in hun code gebeurt. Het is cruciaal voor\
  \ het\u2026"
lastmod: 2024-02-19 22:05:09.825005
model: gpt-4-0125-preview
summary: "Debugoutput afdrukken is in wezen de manier waarop een programmeur een kijkje\
  \ neemt naar wat er in real-time in hun code gebeurt. Het is cruciaal voor het\u2026"
title: Debug-output afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?
Debugoutput afdrukken is in wezen de manier waarop een programmeur een kijkje neemt naar wat er in real-time in hun code gebeurt. Het is cruciaal voor het opsporen van bugs en het begrijpen van de codeflow zonder formele debuggingtools of -sessies op te zetten.

## Hoe:
Laten we dingen naar de console printen:

```Kotlin
fun main() {
    val magischGetal = 42
    println("Het magische getal is $magischGetal")

    debugPrint("Het magische getal in het kwadraat is ${magischGetal * magischGetal}")
}

fun debugPrint(bericht: String) {
    if (BuildConfig.DEBUG) {
        println("DEBUG: $bericht")
    }
}
```
Voorbeelduitvoer:
```
Het magische getal is 42
DEBUG: Het magische getal in het kwadraat is 1764
```
Snel en vies, je ziet je waarden daar meteen in de console.

## Diepere Duik
Het afdrukken naar de console voor debuggen is zo oud als de weg naar Rome. Het is simpel, het is alomtegenwoordig in alle programmeertalen, en het klaart de klus. Maar, het is niet fancy, en in complexe systemen kan te veel output een zooitje zijn.

Alternatieven voor `println` in Kotlin kunnen zijn het gebruik van log-frameworks zoals `Log4j` of Kotlin's ingebouwde `Logging` utility, wat helpt om berichten te filteren op basis van ernstniveaus.

Een nuance in Kotlin, zoals te zien in onze `debugPrint` functie, is om te controleren of we in een debugbuild zitten; Op deze manier verstoppen we onze productielogs niet met onze debugberichten, waardoor onze daadwerkelijke implementaties schoon en gebruiksvriendelijk blijven.

## Zie Ook
- Voor een introductie tot loggen in Kotlin, raadpleeg de officiële documenten: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- JetBrains’ visie op debuggingstrategieën: [IntelliJ IDEA Debugging](https://www.jetbrains.com/help/idea/debugging-code.html)
- Als je Android gebruikt, is de officiële gids voor het gebruik van Logcat onschatbaar: [Android Logcat Documentatie](https://developer.android.com/studio/command-line/logcat)
