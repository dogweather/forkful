---
title:    "Kotlin: Pisanie do standardowego błędu"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu (standard error) jest niezwykle ważną umiejętnością dla programisty. Jest to często używane w celu zapisania informacji o błędach i problemach podczas wykonywania programu. Pozwala to na szybkie i skuteczne diagnozowanie problemów w kodzie.

## Jak to zrobić

Aby napisać do standardowego błędu w języku Kotlin, wystarczy użyć metody `System.err.println()`. Przykłady można znaleźć poniżej:

```Kotlin
val a = 5
val b = 0

try {
    val result = a / b
    System.out.println(result)
} catch (e: ArithmeticException) {
    System.err.println("Dzielenie przez 0 nie jest dozwolone.")
}

```

Output:
```
Dzielenie przez 0 nie jest dozwolone.
```
W powyższym przykładzie, kiedy wartość zmiennej `b` jest równa 0, wywołane zostanie wyjątkowe zdarzenie (ArithmeticException) i komunikat o błędzie zostanie zapisany do standardowego błędu.

## Deep Dive

Aby dogłębniej zrozumieć pisanie do standardowego błędu, warto wiedzieć, że jest to część standardowego strumienia wyjścia w systemie operacyjnym. Jest to niezwykle ważne, ponieważ pozwala na wyodrębnienie informacji o błędach od innych danych wyjściowych, takich jak standardowy strumień wyjścia (standard output). 

Dodatkowo, istnieje możliwość przekierowania informacji zapisanych do standardowego błędu do pliku lub innego strumienia wyjścia. Dzięki temu można dokładnie monitorować i analizować błędy w programie.

## Zobacz również
- [Dokumentacja języka Kotlin - Pisanie do standardowego strumienia błędów](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-output-stream/-system-out/err.html)
- [Poradnik programisty - Pisanie do standardowego błędu w języku Java](https://www.baeldung.com/java-write-to-system-error)
- [Blog o programowaniu - Jak przekierować standardowy błąd do pliku w systemie Linux](https://www.linuxjournal.com/content/redirecting-output-bash)