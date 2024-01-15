---
title:                "Att hitta längden på en sträng"
html_title:           "Kotlin: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man skulle vilja hitta längden på en sträng i en Kotlin-programmering. Det kan till exempel vara för att validera inmatade data eller för att manipulera texten på något sätt.

## Hur gör man

För att hitta längden på en sträng i Kotlin, kan du använda funktionen `length()`. Den används genom att ange strängen du vill undersöka inom parentesen:

```Kotlin
val sträng = "Hej, jag är en sträng"
val längd = sträng.length()

print(längd) // Utskrift: 21
```

Du kan också använda `length`-metoden direkt på en sträng utan att behöva spara den i en variabel:

```Kotlin
val längd = "Jag är en sträng".length()

print(längd) // Utskrift: 17
```

Funktionen `length()` returnerar antalet tecken i en sträng, och alla typer av tecken räknas, inklusive mellanslag och specialtecken.

## Deep Dive

Det kan vara användbart att veta att `length()`-metoden faktiskt är en extension-funktion på strängar, vilket betyder att den är en del av Kotlin's standard library. En extension-funktion kan användas på en typ av objekt för att utöka dess funktionalitet. I fallet med `length()` så utökar den funktionaliteten hos strängar genom att räkna ut antalet tecken.

En intressant sak att notera är att när du använder `length()` på en tom sträng, kommer den att returnera värdet 0, eftersom det inte finns några tecken att räkna.

```Kotlin
val tomSträng = ""
val längd = tomSträng.length()

print(längd) // Utskrift: 0
```

Du kan också använda `length()` i kombination med andra metoder för att manipulera strängar. Till exempel kan du använda den tillsammans med `substring()` för att få ut en del av en sträng baserat på dess längd.

```Kotlin
val sträng = "Det här är en sträng"
val längd = sträng.length()
val delSträng = sträng.substring(4, längd)

print(delSträng) // Utskrift: är en sträng
```

## Se även

- Kotlin Standard Library: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/#length
- Kotlin Basic Types: https://kotlinlang.org/docs/reference/basic-types.html#strings