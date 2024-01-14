---
title:                "Kotlin: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Att använda reguljära uttryck kan vara ett utmärkt sätt att hantera och manipulera text på ett effektivt sätt. Genom att använda reguljära uttryck kan du hitta och ersätta eller manipulera text på ett sätt som vanliga sök- och ersättningsfunktioner inte kan göra.

## Hur man

```Kotlin
// Skapa en reguljär expression för att hitta email-adresser
val regex = Regex("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}")

// Hitta och skriv ut email-adresser från en text
val text = "Kontakta mig på john@example.com"
regex.findAll(text).forEach { matchResult ->
    println("Email: ${matchResult.value}")
}
// Output:
// Email: john@example.com
```

```Kotlin
// Ersätt alla emoticons med texten "emoji"
val emoticons = listOf(":)", ":D", ":(")
val text = "Jag är så glad idag! :)"
var newText = text

// Loopa genom alla emoticons och ersätt dem med texten "emoji"
emoticons.forEach { emoticon ->
    newText = newText.replace(emoticon, "emoji")
}

// Skriv ut den nya texten
println(newText)
// Output:
// Jag är så glad idag! emoji
```

## Deep Dive
När du använder reguljära uttryck i Kotlin bör du alltid använda Regex-klassen. Den erbjuder enkel syntax och många användbara metoder för att söka efter och manipulera text.

En viktig del av reguljära uttryck är specialtecken. Till exempel markerar ett punktum (.) vilken som helst enskilt tecken och en asterisk (*) markerar noll eller fler av det föregående mönstret. Det finns också möjlighet att använda [] för att specificera en mängd av tecken eller använda en rad kontrolltecken som \d för att matcha siffror och \w för att matcha bokstäver.

För att lära dig mer om syntax och alla tillgängliga metoder i Regex-klassen, rekommenderas det att läsa Kotlin dokumentationen för reguljära uttryck.

## Se också
- [Kotlin dokumentation - Regular Expressions](https://kotlinlang.org/docs/regex.html)
- [Mastering Regular Expressions - Book by Jeffrey E.F. Friedl](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124)
- [Regex101 - Online Regular Expression Tester](https://regex101.com/)