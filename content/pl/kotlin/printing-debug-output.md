---
title:                "Kotlin: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Drukowanie informacji debugowania jest kluczowym elementem w procesie programowania Kotlin. Pozwala nam ono na dokładne śledzenie przebiegu działania programu i naprawianie ewentualnych błędów. Zapoznajmy się z tym narzędziem bliżej!

## Jak to zrobić

Aby wydrukować informacje debugowania w Kotlinie, wystarczy użyć funkcji `println()` i umieścić w niej odpowiednie zmienne lub wyrażenia. Na przykład:

```Kotlin
fun main() {
   val x = 5
   val y = 3
   println("Wartość zmiennej x wynosi $x, a wartość zmiennej y wynosi $y.")
}
```

Wynikiem tego kodu będzie wypisanie w konsoli: "Wartość zmiennej x wynosi 5, a wartość zmiennej y wynosi 3." Dzięki temu możemy na bieżąco monitorować wartości naszych zmiennych i łatwiej zidentyfikować ewentualne błędy.

Do drukowania informacji debugowania można również wykorzystać funkcję `log()` z pakietu `android.util`. Ta metoda jest szczególnie przydatna w przypadku aplikacji mobilnych pisanych w Kotlinie. Przykład użycia:

```Kotlin
fun main() {
    val x = 5
    val y = 3
    Log.d("Debugowanie", "Wartość zmiennej x wynosi $x, a wartość zmiennej y wynosi $y.")
}
```

Wynikiem będzie wypisanie w logach aplikacji: "Debugowanie: Wartość zmiennej x wynosi 5, a wartość zmiennej y wynosi 3."

## Głębszy zanurzenie

Jeśli chcemy jeszcze bardziej zaawansować swoje debugowanie, możemy skorzystać z opcji dodawania tagów do informacji drukowanych w logach. Dzięki temu możemy łatwiej zidentyfikować, z jakiej części naszego kodu pochodzi dana informacja. Przykład:

```Kotlin
fun main() {
    val x = 5
    val y = 3
    Log.d("Moja aplikacja", "Wartość zmiennej x wynosi $x, a wartość zmiennej y wynosi $y.")
}
```

W logach pojawi się wtedy: "Moja aplikacja: Wartość zmiennej x wynosi 5, a wartość zmiennej y wynosi 3."

Inną przydatną opcją jest również wykorzystanie poziomów logowania. Możemy je ustawić dla poszczególnych informacji debugowania, co pozwala na lepszą organizację i filtrowanie wyświetlanych w logach danych.

## Zobacz także

- Dokumentacja Kotlina o drukowaniu informacji debugowania: https://kotlinlang.org/docs/tutorials/kotlin-for-py/print-debugging-info.html
- Poradnik o debugowaniu w Kotlinie: https://www.raywenderlich.com/5158-kotlin-debugging-tutorial-getting-started