---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text är operationen att hitta vissa tecken/ord/mönster i en sträng och ersätta dem med något annat. Formulärvalidering, textredigering, datarensning – programmerare gör det ständigt för att manipulera data effektivt.

## Hur gör man:
Här är grundläggande kodexempel för att söka och ersätta text i Kotlin:

```Kotlin
fun main() {
    val text = "Hej världen!"
    val nyText = text.replace("världen", "Sverige")
    println(nyText)  // Skriver ut: Hej Sverige!
}
```
I det här exemplet ersätter vi `världen` med `Sverige` i textsträngen.

## Djupdykning
Sök och ersätt funktionen har varit en grundläggande funktion i programmering ända sedan dess början. I Kotlin görs detta genom `replace`-metoden, men det finns också alternativa metoder såsom `replaceFirst`, `replaceRange` och andra.

Den grundläggande `replace`-metoden tar två parametrar, den första är målsträngen som ska ersättas, och den andra är strängen som ska infogas. 

Själva utförandet av 'replace' funktionen sker via noggrann strängbearbetning och går igenom varje index i strängen för att hitta önskad matchning. Den beaktar inte mellanrum, storlek, eller andra specialtecken om inte uttryckligen angett.

## Se även:
För mer detaljerad information och vidare läsning om hur du hanterar strängar i Kotlin, följ dessa länkar:
1. Officiell Kotlin dokumentation för strängar: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)
2. Tutorial för att förstå `replace`-funktioner i Kotlin: [https://www.programiz.com/kotlin-programming/string](https://www.programiz.com/kotlin-programming/string)
3. Diskussion och exempel på att använda regelbundna uttryck (regex) med Kotlin: [https://www.baeldung.com/kotlin-regex](https://www.baeldung.com/kotlin-regex).