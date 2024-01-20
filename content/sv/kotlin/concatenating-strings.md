---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

String-konkatenering innebär att kombinera två eller flera strängdata tillsammans i en enda sträng. Programmerare gör det för att forma dynamiska meddelanden, bygga filvägar, och generera kod, bland annat.

## Så här gör du: 

För att konkatenera strängar i Kotlin, använd plus-operatorn (`+`) eller `plus` funktionen. Här är hur:

```kotlin
val hello = "Hej"
val world = "Världen"
val message = hello + " " + world // "Hej Världen"
println(message)

// eller

val annanMeddelande = hello.plus(" ").plus(world) // "Hej Världen"
println(annanMeddelande)
```

## Fördjupning:

I äldre versioner av Kotlin, och i vissa andra språk, krävs det mer kod för att konkatenera strängar. Men i Kotlin, tack vare operatör överbelastning, är det mycket enklare. 

Förutom `+` och `plus`, kan du använda `StringBuilder` för att konkatenera strängar vilket är mer lämpligt för långa och komplexa strängar eller när prestanda är viktig.

Implementationsdetaljerna kan variera mellan olika versioner och plattformar, men generellt är `StringBuilder` snabbare när du lägger till många strängar.

## Se också:

- Officiell Kotlin dokumentation på string-konkatenering: [kotlinlang.org/docs/strings.html#string-literals](https://kotlinlang.org/docs/strings.html#string-literals)
  
- En närmare titt på `StringBuilder`: [baeldung.com/kotlin-stringbuilder](https://www.baeldung.com/kotlin-stringbuilder)

- Diskussioner om strängkonkatenering på Stack Overflow: [stackoverflow.com/questions/tagged/kotlin+string-concatenation](https://stackoverflow.com/questions/tagged/kotlin+string-concatenation)