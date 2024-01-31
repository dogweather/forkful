---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck (regex) är textmatchningsmönster. Programmerare använder dem för att söka, validera eller manipulera text effektivt.

## Så Gör Du:
```kotlin
fun main() {
    val text = "Hej, Välkommen till Kotlin!"
    val regex = Regex(pattern = "\\bKotlin\\b")
    val found = regex.containsMatchIn(input = text)

    println("Hittade Kotlin i texten? $found")
    
    val regexReplace = text.replace(Regex("\\bKotlin\\b"), "Java")
    println("Ersättning: $regexReplace")
}
```
Output:
```
Hittade Kotlin i texten? true
Ersättning: Hej, Välkommen till Java!
```

## Fördjupning
Regular expressions härstammar från teoretisk datavetenskap på 1950-talet. Alternativ till regex inkluderar parserbibliotek och inbyggda strängmetoder, men dessa saknar regex' flexibilitet. Regex använder en kompileringstid vid första anropet för bättre prestanda vid upprepade sökningar.

## Se Även
- [Kotlin Dokumentation för Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regex101: Online regex tester och debugger](https://regex101.com/)
- [Mozilla Developer Network Regex Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
