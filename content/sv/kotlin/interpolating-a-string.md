---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stränginterpolation är en process där vi ersätter variabler inuti en sträng med deras faktiska värden. Det hjälper programmerare att skapa mer läsbar och underhållbar kod.

## Så här gör du:
I Kotlin kan du utföra stränginterpolation genom att infoga variabler direkt i strängen med hjälp av "$" tecknet. Nedan är ett kodexempel samt dess utdata.

```Kotlin 
fun main() {
    val name = "Sven"
    val age = 22
    println("Hej $name, du är $age år gammal.")
}
```
När du kör den här koden blir utskriften

``` 
"Hej Sven, du är 22 år gammal."
```
## Djupdykning
Stränginterpolation har sitt ursprung i Unix-skal och Perl. Alternativen till stränginterpolation inkluderar att använda funktionen `format()` eller konkatenering.

I Kotlin ersätts interpolerade strängar direkt av kompilatorn med deras motsvarande `StringBuilder`-operationer. Detta leder till att prestandan för kod som använder stränginterpolation och kod som använder `StringBuilder` är i princip lika.

## Se även
Här är några praktiska länkar till ytterligare läsning om stränginterpolation:
1. Kotlin dokumentation om stränginterpolation: [https://kotlinlang.org/docs/idioms.html#string-interpolation](https://kotlinlang.org/docs/idioms.html#string-interpolation)
2. StackOverflow-tråd om stränginterpolation: [https://stackoverflow.com/questions/42780000/kotlin-string-interpolation](https://stackoverflow.com/questions/42780000/kotlin-string-interpolation)