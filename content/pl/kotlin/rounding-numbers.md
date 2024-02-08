---
title:                "Zaokrąglanie liczb"
aliases:
- pl/kotlin/rounding-numbers.md
date:                  2024-01-26T03:45:43.964278-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb oznacza dostosowywanie ich do najbliższej liczby całkowitej lub do określonego stopnia precyzji. Programiści robią to, aby poprawić czytelność, zmniejszyć wymagania dotyczące miejsca przechowywania lub ponieważ dokładna wartość nie jest krytyczna dla kolejnych obliczeń.

## Jak to zrobić:

W Kotlinie, zaokrąglanie można wykonać za pomocą kilku funkcji, takich jak `roundToInt()`, `roundToDouble()`, oraz używając `BigDecimal` dla większej kontroli:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Wyświetla: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Wyświetla: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Wyświetla: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Wyświetla: 123.5
}
```

## Dogłębna analiza

Historycznie, zaokrąglanie liczb było fundamentalnym pojęciem zarówno w matematyce, jak i w obliczeniach, zaprojektowanym do radzenia sobie z ograniczeniami precyzji numerycznej. We wczesnych czasach informatyki, zaokrąglanie było kluczowe z powodu wysokich kosztów pamięci.

W Kotlinie, zaokrąglanie opiera się na standardowych bibliotekach Java. Opcje zaokrąglenia obejmują `Math.round()`, które zaokrągla do najbliższej liczby całkowitej, oraz `BigDecimal` do niestandardowego zaokrąglania, gdzie można określić skalę i `RoundingMode`.

Każdy `RoundingMode` ma różne zasady postępowania z remisami (kiedy cyfra jest dokładnie pomiędzy opcjami zaokrąglenia). Na przykład, `RoundingMode.HALF_UP` zaokrągla do najbliższego sąsiada, chyba że obaj sąsiedzi są równo oddaleni, w takim przypadku zaokrągla w górę.

## Zobacz również

- Dokumentacja Kotlin na temat [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Dokumentacja Javy Oracle dla [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754) [IEEE Standard 754](https://ieeexplore.ieee.org/document/4610935)
