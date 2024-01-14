---
title:    "Kotlin: Pobieranie długości ciągu znaków"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

W dzisiejszych czasach często spotykamy się z potrzebą operowania na tekście w naszych programach. Może to być analiza wprowadzonego przez użytkownika hasła, sprawdzenie poprawności adresu email czy też przechowywanie imion i nazwisk w bazie danych. W takich przypadkach niezbędne jest znajomość długości danego tekstu. W tym artykule prześledzimy sposób na obliczenie długości stringa w języku Kotlin.

# Jak to zrobić

Najprostszym sposobem na obliczenie długości tekstu jest użycie wbudowanej funkcji `length()` na obiekcie typu `String`. Przykładowy kod wygląda następująco:

```Kotlin
val text = "To jest przykładowy tekst"
println(text.length()) // output: 26
```

Możemy również skorzystać z własnej funkcji, która iteracyjnie będzie przeglądać kolejne znaki tekstu i zliczać ich ilość. Poniżej przykład takiej implementacji:

```Kotlin
fun countLength(text: String): Int {
    var count = 0
    for (char in text) {
        count++
    }
    return count
}

val text = "To jest przykładowy tekst"
println(countLength(text)) // output: 26
```

W obu przypadkach otrzymamy dokładnie taki sam wynik.

# Głębszy przegląd

Zastanawiasz się, jak działają te funkcje i dlaczego są takie proste w użyciu? W przypadku wbudowanej funkcji `length()` odwołujemy się do obiektu `String`, który przechowuje w sobie wartość tekstową. Następnie jest wywoływana metoda `length()`, która sprawdza długość tego tekstu i zwraca odpowiednią wartość. W przypadku funkcji `countLength()` tworzymy własną funkcję, która korzysta z pętli `for` do zliczania znaków w tekście.

# Zobacz również

Jeśli jesteś zainteresowany zapoznaniem się z innymi możliwościami przetwarzania tekstu w języku Kotlin, polecamy Ci zapoznać się z następującymi artykułami:

- [Przetwarzanie tekstu w języku Kotlin](https://kotlinlang.org/docs/basic-idioms.html#string-templates)
- [Manipulacja stringami w Kotlin](https://blog.kotlin-academy.com/strings-manipulation-in-kotlin-e6dec57fe26f)
- [Dlaczego warto wybrać Kotlin do pracy z tekstem](https://try.kotlinlang.org/#/Examples/Digital%20substraction/String%20iterating/String%20iterating.kt)