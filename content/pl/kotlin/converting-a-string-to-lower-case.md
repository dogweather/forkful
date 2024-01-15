---
title:                "Konwersja ciągu znaków na małe litery."
html_title:           "Kotlin: Konwersja ciągu znaków na małe litery."
simple_title:         "Konwersja ciągu znaków na małe litery."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym artykule omówimy proces konwertowania ciągu znaków na małe litery w języku Kotlin. Jest to często wykorzystywana funkcja, ponieważ pozwala na spójne porównywanie tekstów oraz ułatwia przetwarzanie i analizę danych tekstowych.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery w języku Kotlin, możemy skorzystać z metody `toLowerCase()`. Przykładowy kod wygląda następująco:

```Kotlin
val text = "JĘZYK KOTLIN"
val lowerCaseText = text.toLowerCase()
println(lowerCaseText)
```

W tym przypadku, na ekranie zostanie wyświetlony tekst "język kotlin". Metoda `toLowerCase()` przekonwertowała wszystkie litery na małe, a pozostałe znaki pozostawiła bez zmian.

Możemy również zastosować tę metodę bezpośrednio na zmiennej typu `String`:

```Kotlin
var text = "Hello, World!"
text = text.toLowerCase()
println(text)
```

Tutaj efekt będzie taki sam, jednak tekst "Hello, World!" zostanie zmieniony na "hello, world!".

## Głębsza analiza

Warto zauważyć, że metoda `toLowerCase()` działa w oparciu o bieżącą lokalizację, w której jest uruchomiony program. Oznacza to, że dla różnych języków może ona działać inaczej.

Ponadto, Korzystając z metody `toLowerCase()` musimy pamiętać, że w języku Kotlin stringi są niemodyfikowalne - oznacza to, że nie możemy zmienić wartości już istniejącej zmiennej typu `String`. Dlatego warto przypisać wynik metody do zmiennej lub od razu wyświetlić wynik na ekranie.

## Zobacz również

* [Dokumentacja Kotlin - stringi](https://kotlinlang.org/docs/reference/basic-types.html#strings)
* [Przekształcanie ciągów znaków w języku Kotlin](https://www.baeldung.com/kotlin/string-to-lowercase)