---
title:    "Kotlin: Pisanie testów"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy?

Pisanie testów jest ważną częścią procesu programowania, ponieważ umożliwia sprawdzenie poprawności naszego kodu w automatyczny sposób. Jest to szczególnie ważne w przypadku większych projektów, gdzie zmiany w jednej części kodu mogą mieć wpływ na inne części. Dodatkowo, testy pozwalają na szybkie wykrycie błędów i ułatwiają odnajdywanie przyczyny problemów.

## Jak pisać testy w języku Kotlin?

Pisanie testów w języku Kotlin jest bardzo proste i wygodne. Aby to zrobić, należy użyć biblioteki do testowania, takiej jak `JUnit`, a następnie utworzyć klasę z metodami testowymi. Poniżej przedstawione są przykładowe testy wykorzystujące klasę `String`:

```Kotlin
// import biblioteki JUnit
import org.junit.Test
// import funkcji do asercji
import org.junit.Assert.*

// deklaracja klasy z testami
class StringTests {
  // deklaracja metody testującej
  @Test
  fun testLength() {
    // przygotowanie danych wejściowych
    val input = "Ala ma kota"
    // wywołanie metody, którą chcemy przetestować
    val output = input.length()
    // asercja sprawdzająca, czy wynik jest zgodny z oczekiwaniami
    assertEquals(11, output)
  }

  // inny przykład testu
  @Test
  fun testReversed() {
    val input = "Kot ma Alę"
    val output = input.reversed()
    assertEquals("Alę ma Kot", output)
  }
}
```

## Głębszy wgląd w pisanie testów

Ważnym aspektem przy pisaniu testów jest pokrycie jak największej ilości możliwych przypadków. Testy powinny sprawdzać zarówno poprawne działanie kodu, jak i jego reakcję na błędy lub niepoprawne dane wejściowe. Możliwe jest również wykorzystanie asercji specjalnych, które pozwalają na porównanie wyjątków rzucanych przez nasz kod.

Podczas pisania testów ważne jest również wykorzystanie nazewnictwa zgodnie z konwencjami, na przykład nazwa metody testującej powinna zawierać opis tego, co jest testowane.

## Zobacz także

- [Dokumentacja biblioteki JUnit](https://junit.org/junit5/docs/current/user-guide/)
- [Przykładowy projekt z testami w języku Kotlin](https://github.com/junit-team/junit5-samples/tree/master/junit5-jupiter-starter-gradle)