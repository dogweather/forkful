---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:25.717251-07:00
description: "Regulj\xE4ra uttryck (regex) \xE4r ett kraftfullt verktyg f\xF6r textbearbetning,\
  \ som m\xF6jligg\xF6r f\xF6r programmerare att s\xF6ka, matcha och manipulera textstr\xE4\
  ngar med\u2026"
lastmod: '2024-03-13T22:44:37.861504-06:00'
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck (regex) \xE4r ett kraftfullt verktyg f\xF6r textbearbetning,\
  \ som m\xF6jligg\xF6r f\xF6r programmerare att s\xF6ka, matcha och manipulera textstr\xE4\
  ngar med avancerade m\xF6nsters\xF6kningsmetoder."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:


### Grundläggande Matchning
För att kontrollera om en sträng matchar ett specifikt mönster i Kotlin kan du använda metoden `matches` i `Regex`-klassen.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Utdata: true
```

### Hitta och Extrahera Delar av Strängen
Om du vill hitta delar av en sträng som matchar ett mönster, låter Kotlin dig iterera över alla träffar:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Dagens datum är 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Utdata: 07/09/2023
```

### Ersätta Text
Att ersätta delar av en sträng som matchar ett mönster är enkelt med `replace`-funktionen:

```kotlin
val input = "Användarnamn: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Utdata: Användarnamn: userXXX
```

### Dela Strängar
Dela en sträng i en lista, med hjälp av ett regex-mönster som avgränsare:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Utdata: [1, 2, 3, 4, 5]
```

### Tredjepartsbibliotek: Kotest
[Kotest](https://github.com/kotest/kotest) är ett populärt Kotlin-testbibliotek som utökar Kotlins inbyggda regex-stöd, särskilt användbart för validering i testfall.

```kotlin
// Förutsatt att Kotest är tillagt till ditt projekt
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Detta kommer att klara testet om input matchar e-postmönstret.
```

Genom att integrera reguljära uttryck i dina Kotlin-applikationer kan du utföra sofistikerad textbehandling effektivt. Oavsett om du validerar användarinmatning, extraherar data eller transformerar strängar, erbjuder regex-mönster en robust lösning.
