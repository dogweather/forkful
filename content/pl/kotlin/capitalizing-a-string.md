---
title:                "Kotlin: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać kapitalizacji w ciągu znaków

Kapitalizacja jest ważnym elementem w wielu programach, szczególnie w aplikacjach, które odwołują się do języka naturalnego, takich jak strona internetowa czy aplikacja do przetwarzania tekstu. Poprawna kapitalizacja pomaga w czytelności tekstu oraz wyróżnieniu ważnych elementów. W tym poście pokażemy, jak w prosty sposób zastosować kapitalizację w języku Kotlin.

## Jak to zrobić

Kapitalizacja występuje w różnych formach, ale najczęstszym jej zastosowaniem jest zmiana pierwszej litery słowa na wielką. W języku Kotlin istnieje prosta funkcja, która umożliwia nam dokonanie tego szybko i bezproblemowo.

```Kotlin
fun capitalize(str: String): String {
    return str.capitalize()
}
```

W powyższym przykładzie wykorzystaliśmy funkcję `capitalize()`, która jako argument przyjmuje ciąg znaków i zwraca ten ciąg z pierwszą literą zamienioną na wielką. 

Przykładowe wywołanie funkcji może wyglądać następująco:

```Kotlin
print(capitalize("kotlin"))
```

Output: Kotlin

Funkcja `capitalize()` zawsze zwróci ciąg znaków z pierwszą literą będącą wielką, nawet jeśli słowo zaczyna się od małej litery. Na przykład:

```Kotlin
print(capitalize("każdy"))
```

Output: Każdy

### Obsługa wyjątków

W niektórych przypadkach, na przykład gdy przetwarzamy słowa wycinane z dłuższego tekstu, może się zdarzyć, że wyraz będzie zaczynał się od znaku inny niż litera. W takiej sytuacji funkcja `capitalize()` nie zadziała poprawnie, ale możemy to łatwo naprawić poprzez sprawdzenie pierwszego znaku i ewentualną zmianę go na literę.

```Kotlin
fun capitalize(str: String): String {
    if(str[0].isLetter()) {
        return str.capitalize()
    }
    return str[0].toUpperCase() + str.substring(1)
}
```

Funkcja ta najpierw sprawdza, czy pierwszy znak w ciągu znaków jest literą. Jeśli tak, to wykorzystuje funkcję `capitalize()`, w przeciwnym razie zamienia pierwszy znak na wielką literę oraz dodaje do niego resztę ciągu (bez pierwszego znaku).

## Dogłębne omówienie

Kapitalizacja jest często wykorzystywana do poprawy czytelności tekstu i wyróżniania ważnych elementów, ale warto pamiętać, że w różnych językach istnieją różnice w wykorzystywaniu kapitalizacji. Na przykład, w języku polskim obowiązuje kapitalizacja inicjałowa - wielka litera jest stosowana tylko na początku zdania oraz w nazwach własnych.

Język Kotlin oferuje wiele innych funkcji związanych z kapitalizacją, takich jak `decapitalize()` (zamiana pierwszej litery na małą), `upperCase()` (zamiana wszystkich liter na wielkie) czy `lowerCase()` (zamiana wszystkich liter na małe). W zależności od potrzeb, możemy wykorzystywać różne funkcje do dostosowania naszego tekstu.

## Zobacz także

- Dokumentacja języka Kotlin: https://kotlinlang.org/docs/reference/
- Wprowadzenie do języka Kotlin: https://kotlinlang.org/docs/tutorials/getting-started.html
- Wprowadzenie do funkcji w Kotlinie: https://kotlinlang.org/docs/reference/functions.html