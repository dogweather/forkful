---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Kotlin: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
För att göra kod mer effektiv och lättläst, kan man behöva ta bort vissa karaktärer som uppfyller ett visst mönster i sitt utseende. Detta kan också vara användbart för att filtrera ut oönskade data eller för att förbättra prestandan i ett program.

## Hur man gör det
För att ta bort karaktärer som matchar ett visst mönster i Kotlin, kan du använda en inbyggd funktion som heter `replace()`.

```Kotlin
val text = "H■e■j d■a■r■!" 
val newText = text.replace("■", "") // "Hej där!"
```

För att ta bort flera karaktärer som matchar ett mönster, kan du använda en reguljäruttryck i `replace()`.

```Kotlin
val text = "K◘o÷t↔i↓l▲n" 
val newText = text.replace(Regex("[◘÷↔▲]"), "") // Kotlin
```

Du kan även använda `removeRange()` för att ta bort specifika index av karaktärer från en sträng.

```Kotlin
val text = "Hej där!" 
val newText = text.removeRange(4..7) // "Hej!"
```


## Djupdykning
`replace()` ger möjlighet att använda både enkla strängar och reguljäruttryck för att ersätta karaktärer, vilket ger en mer flexibel användning beroende på dina behov. Reguljäruttryck kan också användas för att hitta och ersätta specifika mönster i en sträng.

Mönster i strängen som bara förekommer en gång kommer att tas bort, medan mönster som återkommer flera gånger i strängen kommer att ersättas med det specifika tecknet som anges i `replace()`.

## Se även
- [RegExp i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-reg-exp/)
- [Kotlin - Stränghantering](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)