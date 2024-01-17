---
title:                "Att använda reguljära uttryck"
html_title:           "Kotlin: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Vad och varför?
Användning av reguljära uttryck (regular expressions) inom programmering handlar om att söka efter och matcha mönster i textsträngar. Det kan användas för att göra sökningar, ersättningar eller validering av text. Programmerare använder dessa uttryck för att effektivt hantera och manipulera data i sina applikationer.

# Hur gör man?
För att använda reguljära uttryck i Kotlin, behöver du importera Regex-paketet in i ditt program. Sedan kan du använda Regex.bitestate() -funktionen för att skapa ett reguljärt uttryck och sedan använda find(), match() eller replace() för att manipulera texten. Här är ett exempel på att hitta första förekomsten av ett mönster och ersätta den med en annan text:

```Kotlin
val text = "Hello, World!"
val pattern = Regex("[H|h]ello")
println(pattern.replace(text, "Hi"))
```
Output: Hi, World!

# Deep Dive
Reguljära uttryck har funnits sedan 1950-talet och används inom många programmeringsspråk som Java, Python och Perl. De är väldigt effektiva för att söka och manipulera text, men vissa programmerare föredrar att använda String-metoder istället för reguljära uttryck för enklare uppgifter.

# Se även
- [Kotlins dokumentation om reguljära uttryck](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [En grundlig introduktion till reguljära uttryck av Regular-Expressions.info](https://www.regular-expressions.info)
- [En interaktiv lektion om reguljära uttryck av RegexOne](https://regexone.com/)