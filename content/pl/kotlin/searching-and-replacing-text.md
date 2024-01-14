---
title:                "Kotlin: Wyszukiwanie i zastępowanie tekstu."
simple_title:         "Wyszukiwanie i zastępowanie tekstu."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest jedną z najważniejszych umiejętności, a znajomość różnych języków programowania jest niezbędna do sukcesu. Jednym z najpopularniejszych języków programowania jest Kotlin, który oferuje wiele przydatnych funkcji, w tym funkcje do wyszukiwania i zamiany tekstu. W tym artykule dowiesz się, dlaczego ta funkcja jest tak przydatna i jak jej używać.

## Jak to zrobić

Aby przeprowadzić wyszukiwanie i zamianę tekstu w biedronce, musimy użyć dwóch funkcji: `replace()` i `replaceFirst()`.

```Kotlin
fun main() {
    val text = "Jestem biedronką"
    println(text.replace("biedronką", "motylem"))
    println(text.replaceFirst("j", "M"))
}
```

**Output:**
```
Jestem motylem
Mestem biedronką
```

Funkcja `replace()` znajduje wszystkie wystąpienia podanego tekstu i zastępuje je nowym tekstem. Natomiast funkcja `replaceFirst()` tylko pierwsze wystąpienie podanego tekstu. Obie funkcje zwracają nowy łańcuch znaków, więc musimy go wyświetlić za pomocą funkcji `println()`.

## Głębsze zagłębienie

Funkcje `replace()` i `replaceFirst()` umożliwiają również wyrażenia regularne, co czyni je jeszcze bardziej wszechstronnymi. Możemy na przykład zastosować wyrażenie regularne, aby znaleźć wszystkie litery w tekście i zamienić je na wielkie litery.

```Kotlin
fun main() {
    val text = "Jestem biedronką"
    println(text.replace(Regex("[a-z]"), { it.value.toUpperCase() }))
}
```

**Output:**
```
JESTEM BIEDRONKĄ
```

Inną przydatną funkcją jest `replaceAfter()`, która umożliwia zamianę tekstu po określonym ciągu znaków. Na przykład, jeśli chcemy zamienić słowo "biedronką" na "jeżem", ale tylko po słowie "jestem", możemy to zrobić w ten sposób:

```Kotlin
fun main() {
    val text = "Jestem biedronką, jestem szczęśliwy"
    println(text.replaceAfter("jestem", "jeżem"))
}
```

**Output:**
```
Jestem jeżem, jestem szczęśliwy
```

Pamiętaj, że w przypadku braku dopasowania, funkcje `replace()` i `replaceAfter()` zwrócą po prostu niezmieniony łańcuch znaków.

## Zobacz również

- [Kotlin funkcje tekstowe](https://kotlinlang.org/docs/reference/strings.html#string-manipulation) 
- [Tutorial: Wprowadzenie do wyrażeń regularnych w Kotlin](https://www.kotlindevelopment.com/introduction-regex-kotlin/)