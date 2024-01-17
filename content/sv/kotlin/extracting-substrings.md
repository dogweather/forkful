---
title:                "Extrahera substrängar"
html_title:           "Kotlin: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Strängutvinning är processen att extrahera en del av en sträng baserat på ett givet kriterium. Programmerare använder detta för att få tillgång till specifika delar av data som är intressant för deras program.

## Hur man gör:

```Kotlin
val str = "Det här är en svensk mening."

// Hämta en delsträng från index 10 till slutet av strängen.
val del1 = str.substring(10)

// Hämta en delsträng från index 0 till 9.
val del2 = str.substring(0, 9)

// Hämta en delsträng från index 4 till 8.
val del3 = str.substring(4, 8)

// Output: svensk mening.
print("$del1 $del2 $del3")
```

## Djupdykning:

Strängutvinning är en grundläggande del av programmering och har funnits sedan starten av programmeringsspråk. Det finns flera alternativ till `substring()` metoden, såsom `slice()` eller `substr()`. Det finns också implementeringsdetaljer att beakta, då vissa programspråk använder nollindexering medan andra använder ett-indexering.

## Se även:

- [Kotlin String Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [String manipulation techniques in programming](https://www.geeksforgeeks.org/string-manipulation-techniques-in-programming/)
- [Substring in Java vs Kotlin](https://www.baeldung.com/java-vs-kotlin-substring)