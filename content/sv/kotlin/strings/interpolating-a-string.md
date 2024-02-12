---
title:                "Interpolera en sträng"
aliases: - /sv/kotlin/interpolating-a-string.md
date:                  2024-01-20T17:51:14.333197-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringinterpolering är när du plockar in variabler eller uttryck direkt i en textsträng. Det gör din kod mer läsbar och effektiv eftersom du slipper klumpiga plusoperatorer för att bygga dina strängar.

## Så Här Gör Du:
I Kotlin använder du dollar-tecken `$` följt av variabelnamnet, eller `${}` för uttryck. 

```Kotlin
fun main() {
    val name = "Oscar"
    val age = 30
    println("Hej, jag heter $name och är $age år gammal.")

    val plånbok = arrayOf(20, 50, 100)
    println("Jag har ${plånbok.size} sedlar i min plånbok.")
}
```
Output:
```
Hej, jag heter Oscar och är 30 år gammal.
Jag har 3 sedlar i min plånbok.
```

## Djupdykning
Förr var man tvungen att använda konkatenation med `+` eller `StringBuilder` för att sätta ihop strängar. Med Kotlin 1.0 kom stringinterpolering, och det förenklade skrivandet av dynamisk text. Alternativ till stringinterpolering är att använda formateringsmetoder som `String.format()`, vilket kan vara mer lämpligt för komplex formatering. När det gäller implementation, konverterar Kotlin-kompilatorn dina interpolerade strängar till `StringBuilder`-operationer under huven, vilket gör det effektivt.

## Se Också
- Kotlin dokumentation om stringinterpolering: [https://kotlinlang.org/docs/basic-syntax.html#string-templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- `String.format()` i Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/format.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/format.html)
- `StringBuilder` klassen i Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/index.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/index.html)
