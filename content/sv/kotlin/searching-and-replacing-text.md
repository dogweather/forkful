---
title:                "Sökning och ersättning av text"
html_title:           "Kotlin: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text i en kodfil kan vara ett användbart verktyg för att effektivt göra ändringar på flera ställen samtidigt eller för att upprätthålla enhetlighet i kodstilen. Det är också ett vanligt sätt att fixa vanliga fel som stavfel eller föråldrade variabelnamn.

## Hur man gör

Sökning och ersättning i Kotlin kan göras med hjälp av den inbyggda "replace()" funktionen. Detta kräver två parametrar, en söksträng och en ersättningssträng, som används för att hitta och ersätta text i en given sträng. Se nedan för ett exempel:

```Kotlin
var text = "Välkommen till Kotlin!"
var ersättning = text.replace("Kotlin", "Svenska")

println(ersättning)
```

Output:

```
Välkommen till Svenska!
```

Man kan också ange ett tredje argument för att specificera hur många gånger söksträngen ska ersättas, som visas i exemplet nedan:

```Kotlin
var text = "Kotlin är ett Java-baserat programspråk."
var ersättning = text.replace("Java", "kompilera", true)

println(ersättning)
```

Output:

```
Kotlin är ett kompilera-baserat programspråk.
```

## Djupdykning

I Kotlin kan man även använda reguljära uttryck för att söka och ersätta text. Detta ger en mer avancerad sökning med möjlighet att hantera mönster och specialtecken. Med hjälp av metoden "replaceFirst()" kan man också ange en lambda-funktion som bestämmer vad som ska ersättas med. Exempelvis:

```Kotlin
var text = "121, 244, 365"
var ersättning = text.replaceFirst(Regex("[0-9]{3}"), { it.value.toInt() / 2 })

println(ersättning)
```

Output:

```
60, 122, 365
```

En annan funktion som kan vara användbar är "replaceBefore()" och "replaceAfter()", som ersätter texten före eller efter ett visst index eller en viss teckenföljd. Det finns också andra liknande funktioner som "replaceRange()" och "replaceBeforeLast()". Man kan läsa mer om dessa och deras syntax på den officiella Kotlin-dokumentationen.

## Se även

- [Kotlin replace() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Kotlin regular expressions](https://kotlinlang.org/docs/regular-expressions.html)
- [Kotlin string manipulation](https://kotlinlang.org/docs/basic-syntax.html#string-templates)