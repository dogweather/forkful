---
title:                "Kotlin: Pisanie testów"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Kotlinie?

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Pomimo tego, że może wydawać się czasochłonnym zadaniem, w rzeczywistości zapewnia wiele korzyści. Dzięki testowaniu możemy mieć większą pewność, że nasz kod jest wolny od błędów i działa poprawnie. W tym artykule dowiesz się, dlaczego warto pisać testy w języku Kotlin.

## Jak pisać testy w Kotlinie?

Poniżej zamieszczam przykładowy kod w języku Kotlin, który pokazuje jak napisać prosty test funkcji `addNumbers()`. Tworzymy klasę `Calculator` zawierającą funkcję `addNumbers()` oraz klasę `CalculatorTest` zawierającą test tej funkcji.

```Kotlin
class Calculator{
    fun addNumbers(num1: Int, num2: Int): Int{
        return num1 + num2
    }
}

class CalculatorTest{
    val calculator = Calculator()
    
    @Test
    fun `should return correct sum of two numbers`(){
        val result = calculator.addNumbers(2, 4)
        assertEquals(6, result)
    }
}

```

Output po uruchomieniu testu:
```
PASSED: should return correct sum of two numbers
```

W tym przykładzie użyliśmy biblioteki `JUnit` oraz funkcji `assertEquals()` do porównania wyniku z oczekiwaną wartością. Dzięki temu testowi możemy upewnić się, że nasza funkcja `addNumbers()` działa poprawnie.

## Deep Dive: Więcej informacji o pisaniu testów w Kotlinie

Język Kotlin oferuje specjalną składnie dla testowania, która pozwala pisać bardziej czytelne i zwięzłe testy. Przykładem tego jest użycie operatora `infix` do tworzenia testów o bardziej naturalnym wyglądzie.

W poniższym kodzie testujemy funkcję `multiplyNumbers()` oraz używamy operatora `infix` do zweryfikowania, czy wynik jest większy od 10.

```Kotlin
@Test
fun `should return correct multiplication of two numbers`(){
    val result = calculator.multiplyNumbers(3, 5)
    result should be greater than 10
}
```

Kotlin oferuje również wbudowane metody do tworzenia asercji z różnymi typami danych, co znacznie ułatwia pisanie testów.

## Zobacz również

- [Dokumentacja JUnit w języku Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.test/index.html)
- [Poradnik pisania testów w Kotlinie](https://medium.com/@zsmb13/beginners-guide-to-writing-tests-in-kotlin-e426e0ab2e5d)
- [Kurs Kotlin od podstaw z sekcją o testowaniu](https://www.udemy.com/course/kotlin-curso-completo/)

Dzięki testowaniu możemy mieć większą pewność, że nasz kod jest poprawny i działa zgodnie z oczekiwaniami. Mam nadzieję, że ten artykuł pomógł Ci zrozumieć dlaczego warto pisać testy w języku Kotlin oraz jak można to zrobić. Pamiętaj, że im więcej testów napiszesz, tym większa będzie szansa na znalezienie i naprawienie błędów w kodzie.