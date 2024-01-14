---
title:    "Clojure: Znajdowanie długości ciągu znaków"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś zainteresowałby się znajdowaniem długości łańcucha? Odpowiedź jest dość prosta - jest to bardzo powszechne zadanie w programowaniu, szczególnie w językach takich jak Clojure. Wiele aplikacji i algorytmów wymaga operacji na łańcuchach, a znajomość sposobów na ich manipulowanie jest niezbędna do efektywnego pisania kodu.

## Jak to zrobić

Aby znaleźć długość łańcucha w Clojure, możemy skorzystać z funkcji `count`. Przyjmie ona kolejno wszystkie elementy łańcucha i zwróci liczbę reprezentującą jego długość.

```Clojure
(count "Hello")
;; Output: 5
```

Możemy również wykorzystać tę funkcję do znalezienia długości listy zawierającej łańcuchy:

```Clojure
(count ["Hello" "World"])
;; Output: 2
```

Funkcja `count` działa również na innych typach danych, więc nie musimy się martwić o to, czy jest to łańcuch czy inny obiekt.

## Głębsze zagłębienie

Podczas tworzenia aplikacji w Clojure, często będziemy potrzebować informacji o długości łańcucha, aby wykonać odpowiednie operacje. Jednakże, zależnie od kontekstu, pojęcie długości może różnić się. Na przykład, w przypadku liczb, długością może być liczba cyfr, a w przypadku listy - ilość jej elementów. Dlatego właśnie funkcja `count` jest tak przydatna - automatycznie dostosowuje się do typu danych, na którym jest użyta.

Możemy także wykorzystać funkcję `count` w połączeniu z innymi funkcjami dostępnymi w języku Clojure, takimi jak `str`, aby uzyskać dokładnie to, czego potrzebujemy. Przykładowo, aby wyświetlić informację o długości danego łańcucha, możemy wpisać:

```Clojure
(str "Długość łańcucha wynosi: " (count "Hello"))
;; Output: "Długość łańcucha wynosi: 5"
```

## Zobacz również

- [Dokumentacja funkcji `count` w języku Clojure](https://clojuredocs.org/clojure.core/count)
- [Inne przydatne funkcje do manipulowania łańcuchami w Clojure](https://www.tutorialspoint.com/clojure/clojure_string_manipulation.htm)