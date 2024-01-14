---
title:                "Kotlin: Sökning och ersättning av text"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och byta ut text är en vanlig uppgift för programmerare. Det kan vara frustrerande att behöva ändra på flera ställen i koden när man bara vill ändra en bit text. Med Kotlin finns det en enkel lösning för detta problem.

## Hur man gör

För att söka och byta ut text i Kotlin, kan man använda sig av String-klassen och dess inbyggda funktioner för sökning och ersättning. Här är ett enkelt exempel:

```Kotlin
val text = "Välkommen till min blogg!"
val nyText = text.replace("Välkommen", "Hej")
println(nyText)
```

Detta kommer att ge följande output:

```Kotlin
Hej till min blogg!
```

Som man kan se, använder vi funktionen "replace" för att byta ut en bit text mot en annan. Funktionen tar två parametrar, den första är den text som ska bytas ut och den andra är den nya texten.

Det finns också andra funktioner för sökning och ersättning som kan vara användbara i olika situationer. Till exempel, om man vill byta ut flera olika delar av en text, kan man använda sig av funktionen "replaceRange":

```Kotlin
val text = "Kotlin är ett fantastiskt programmeringsspråk!"
val nyText = text.replaceRange(35, 46, "språk")
println(nyText)
```

I detta exempel byter vi ut "programmering" mot "språk". Funktionen "replaceRange" tar tre parametrar, startpositionen (35), slutpositionen (46) och den nya texten.

## Djupdykning

Om man vill gå ännu djupare, finns det en mängd olika sätt att söka och byta ut text i Kotlin. Man kan till exempel använda sig av reguljära uttryck för en mer avancerad sökning. Man kan också använda sig av olika funktioner för att hantera stora datamängder och effektivisera sökningen och ersättningen. Det finns också olika bibliotek och tredjepartsverktyg som kan hjälpa till med detta ändamål.

## Se även

- [Dokumentation för String-klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Reguljära uttryck i Kotlin](https://kotlinlang.org/docs/regex.html)
- [Effektiv sökning och ersättning med Kotlin](https://blog.kotlin-academy.com/effective-search-replace-mechanisms-in-kotlin-dd65b9b9f3f6)