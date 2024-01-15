---
title:                "Sammanslagning av strängar"
html_title:           "Kotlin: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför

Att kunna konkaternera strängar är en viktig del av att skriva effektiv kod. Det kan hjälpa till att göra din kod lättläslig och mer organiserad.

# Så här

Det finns flera sätt att konkaternera strängar i Kotlin, beroende på dina specifika behov.

Först och främst kan du använda tecknet `+` för att enkelt lägga till en sträng till en annan. Till exempel:

```Kotlin
val förnamn = "Anna"
val efternamn = "Svensson"
val fullständigtNamn = förnamn + " " + efternamn

println(fullständigtNamn) // Skriver ut "Anna Svensson"
```

Du kan också använda `+=` operatorn för att lägga till en sträng på slutet av en befintlig sträng. Till exempel:

```Kotlin
var mening = "Jag gillar att"
mening += " läsa böcker."

println(mening) // Skriver ut "Jag gillar att läsa böcker."
```

Om du behöver kombinera flera strängar i en lista kan du använda funktionen `joinToString`. Denna funktion tar en separatteckenparameter (som standard är det ett mellanslag) och slår ihop alla elementen i listan till en enda sträng. Till exempel:

```Kotlin
val frukter = listOf("äpple", "banan", "citron")
val fruktsallad = frukter.joinToString(" och ")

println(fruktsallad) // Skriver ut "äpple och banan och citron"
```

# Djupdykning

Om du vill vara mer effektiv med att konkaternera strängar, kan du använda `StringBuilder` klassen. Detta är en specialiserad klass för att bygga och manipulera strängar. Exempel på användning av `StringBuilder` är:

```Kotlin
val minText = StringBuilder("Hej")

minText.append(" på dig!") // Lägger till " på dig!" på slutet av strängen
minText.insert(3, " där") // Infogar "där" på index 3 (vilket blir "Här på dig!")

println(minText) // Skriver ut "Här på dig!"
```

Det är också viktigt att använda sig av string templates när du konkaternerar värden i en sträng. Med string templates kan du enkelt infoga variabler eller uttryck inuti en sträng utan att behöva använda `+` eller `+=` operatorn. Till exempel:

```Kotlin
val vikt = 56
val längd = 167

println("Min vikt är $vikt kg och min längd är $längd cm.") // Skriver ut "Min vikt är 56 kg och min längd är 167 cm."
```

# Se även

- [Kotlin String Reference](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Kotlin String Templates](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)