---
date: 2024-01-20 17:35:19.671365-07:00
description: "Hur g\xF6r man: Stringkonkatenering har funnits s\xE5 l\xE4nge vi har\
  \ programmerat. I tidigare spr\xE5k som C anv\xE4ndes funktioner som `strcat()`\
  \ men kunde skapa\u2026"
lastmod: '2024-04-05T21:53:39.199710-06:00'
model: gpt-4-1106-preview
summary: "Stringkonkatenering har funnits s\xE5 l\xE4nge vi har programmerat."
title: "Sammanslagning av str\xE4ngar"
weight: 3
---

## Hur gör man:
```kotlin
fun main() {
    val hej = "Hej "
    val varlden = "världen!"
    val halsning = hej + varlden  // Använder '+' operator
    println(halsning) // Skriver ut "Hej världen!"

    val namn = "Sven"
    val meddelande = "$hej$namn!" // Använder string templates
    println(meddelande) // Skriver ut "Hej Sven!"

    val ord = listOf("Kotlin", "är", "roligt!")
    val mening = ord.joinToString(" ") // Använder joinToString-funktionen
    println(mening) // Skriver ut "Kotlin är roligt!"
}
```

## Fördjupning
Stringkonkatenering har funnits så länge vi har programmerat. I tidigare språk som C användes funktioner som `strcat()` men kunde skapa säkerhetsproblem om inte hanterat varsamt. I moderna språk som Kotlin görs det säkrare och enklare med operatorer och string templates.

När det gäller alternativ, kan bygga strängar med `StringBuilder` vara effektivare för längre eller komplexa strängoperationer. Kotlin hanterar stränginterpolering och konkatenering smart bakom kulisserna så att prestandan generellt är bra även utan `StringBuilder`.

Implementation kan variera mellan olika programmeringsspråk. Kotlin kompilerar strängkonkatenering till Java Bytecode som använder `StringBuilder` under huven vid behov för att effektivisera sammansättningen.

## Se också
- [Kotlin Dokumentation om String Templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Oracle Java docs om StringBuilder](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [StackOverflow diskussion om String Performance i Kotlin](https://stackoverflow.com/questions/4645020/performance-tuning-the-stringbuilder-in-java)
