---
title:    "Kotlin: Användning av reguljära uttryck"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck, eller "regular expressions", är ett kraftfullt verktyg inom programmering som kan hjälpa till att hitta och manipulera textsträngar på ett effektivt sätt. Genom att använda reguljära uttryck kan du spara tid och arbete när du arbetar med textbaserade data.

## Hur man gör det

För att använda reguljära uttryck i Kotlin, behöver du importera reguljära uttrycksbiblioteket med hjälp av `import java.util.regex.*`. Sedan kan du använda funktionen `Regex(pattern)` för att skapa ett reguljärt uttryck, där "pattern" är det mönster som du vill matcha. Se följande exempel:

```Kotlin
import java.util.regex.*

val text = "Hej Bloggläsare!" // Vi söker efter "Blogg"
val regex = Regex("Blogg")
val match = regex.find(text) // "find" söker efter första matchningen i strängen
print(match?.value) // Output: "Blogg"
```

För att söka efter flera matchningar, kan du använda funktionen `findAll(text)` istället för `find(text)`. Du kan också använda reguljära uttryck tillsammans med funktioner som `replace` och `split` för att manipulera strängar på olika sätt. Exempel:

```Kotlin
val text = "123abc456def789ghi"
val numbers = Regex("\\D+").replace(text, "") // Ta bort allt utom siffror
val letters = Regex("\\d+").replace(text, "") // Ta bort alla siffror
print(numbers) // Output: "123456789"
print(letters) // Output: "abcdefghi"
```

## Fördjupning

Reguljära uttryck använder speciella tecken och strängar för att matcha olika mönster i text. Till exempel matchar `.` vilket tecken som helst, `+` matchar ett eller flera av föregående tecken och `\d` matchar en siffra. Det finns många fler specialtecken och kombinationer som du kan använda för att skapa mer avancerade reguljära uttryck. 

En annan viktig aspekt av reguljära uttryck är så kallade "capturing groups". Dessa låter dig extrahera specifika delar av en matchning istället för hela matchningen. Du kan använda parenteser `()` för att skapa en "capturing group". Exempel:

```Kotlin
val text = "Hej, mitt namn är Adam"
val regex = Regex("Hej, mitt namn är (\\w+)")
val match = regex.find(text)
print(match?.groupValues?.get(1)) // Output: "Adam"
```

För att lära dig mer om reguljära uttryck och deras olika användningsområden, kan du kolla in nedanstående länkar:

## Se även

- [Kotlin dokumentation om reguljära uttryck](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutorial om reguljära uttryck på codecademy](https://www.codecademy.com/learn/learn-regex)
- [RegExr - Interaktivt verktyg för att testa reguljära uttryck](https://regexr.com/)