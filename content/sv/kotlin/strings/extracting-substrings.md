---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:46:00.372680-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Extrahering av substrängar innebär att plocka ut specifika delar av en sträng. Programmerare gör detta för att bearbeta eller analysera text baserat på behov - såsom att dra ut användarnamn från e-postadresser eller hämta specifika data från en sträng med logginformation.

## Hur gör man:
```Kotlin
fun main() {
    val sträng = "Hejsan Sverige!"
    val subSträng = sträng.substring(7, 14)
    
    println("Originalsträngen: $sträng")
    println("Extraherad substräng: $subSträng")
}

// Output:
// Originalsträngen: Hejsan Sverige!
// Extraherad substräng: Sverige
```

Ett annat exempel, med användning av en range:
```Kotlin
fun main() {
    val sträng = "Fantastisk kodning är kul!"
    val range = 12..16
    val subSträng = sträng.substring(range)
    
    println("Substräng med range: $subSträng")
}

// Output:
// Substräng med range: kodning
```

## Djupdykning
Substräng extraktion har varit ett grundläggande verktyg sedan tidig programmering. Det är inte unikt för Kotlin och varje modernt programmeringsspråk har någon form av denna funktionalitet. I Kotlin finns det flera metoder för att extrahera substrängar: `substring` med start- och stopindex eller med en `IntRange`.

Detaljer in implementation: Kotlin's `substring` använder interna Java-funktioner då Kotlin körs på JVM. Skillnaden i prestanda är ofta försumbar men kan bli relevant i program som hanterar extremt stora strängar eller utför många sådana operationer.

Alternativen till `substring` inkluderar regex (reguljära uttryck) för mer komplexa mönsterutsökningar och `split` för att dela upp strängar vid specificerade avgränsare.

## Se Also
- [Kotlin's officiella dokumentation om substrängar](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Reguljära uttryck i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Split-funktion i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/split.html)
