---
title:                "Praca z liczbami zespolonymi"
aliases:
- /pl/kotlin/working-with-complex-numbers/
date:                  2024-01-26T04:43:03.728446-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Liczby zespolone rozszerzają nasz system liczbowy o pierwiastki kwadratowe z liczb ujemnych, gdzie 'urojona' jednostka i równa się pierwiastkowi kwadratowemu z -1. Programiści używają ich w dziedzinach takich jak inżynieria, fizyka i przetwarzanie sygnałów, ponieważ świetnie nadają się do modelowania fal, oscylacji i wszystkiego, co się obraca.

## Jak to zrobić:

Zdefiniujmy podstawową klasę liczby zespolonej w Kotlinie:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Wyjście: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Wyjście: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Wyjście: a * b = (-5.0 + 10.0i)
}
```

## Szczegółowa analiza

Liczby zespolone zostały po raz pierwszy wspomniane w XVI wieku, rozwiązując równania sześcienne, które nie miały rzeczywistych rozwiązań. Inżynieria i fizyka w znacznym stopniu korzystają z liczb zespolonych do analizy obwodów prądu przemiennego i przebiegów falowych. Alternatywnie, można użyć biblioteki takiej jak `koma` lub `ejml` Kotlin dla pracy z dużymi obciążeniami.

Operacje na liczbach zespolonych odzwierciedlają liczby rzeczywiste, ale z uwzględnieniem jednostki urojonej. Mnożenie, na przykład, podąża za własnością rozdzielności, pamiętając, że `i^2 = -1`. Ta jednostka urojona pozwala nam reprezentować wielowymiarowe liczby, kluczowe w różnych obliczeniach naukowych.

## Zobacz także

Biblioteki matematyczne Kotlin:

- [koma](https://koma.kyonifer.com/): Biblioteka do obliczeń naukowych dla Kotlina.

Dalsze czytanie o liczbach zespolonych:

- [Wikipedia: Liczby zespolone](https://pl.wikipedia.org/wiki/Liczba_zespolona)
