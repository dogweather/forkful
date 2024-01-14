---
title:    "Clojure: Zmiana pierwszej litery na wielką w ciągu znaków"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Ciągły rozwój programowania funkcyjnego, a w szczególności popularność języka Clojure, sprawiają że programiści często napotykają na nowe wyzwania. Jednym z prostych problemów jest kapitalizowanie łańcuchów znaków. W tym artykule dowiesz się dlaczego jest to przydatna umiejętność i jak możesz to zrobić w języku Clojure.

## Jak to zrobić

Kapitalizacja łańcucha znaków polega na zmianie pierwszej litery wyrazu na wielką. W języku Clojure możemy to osiągnąć przy użyciu funkcji `capitalize`. Przyjmujemy w niej argument - łańcuch znaków, a funkcja zwraca nowy łańcuch z pierwszą literą zamienioną na wielką.

```Clojure
(capitalize "hello world")
```
Output: "Hello world"

Gdybyśmy chcieli kapitalizować wszystkie wyrazy w łańcuchu, możemy użyć funkcji `clojure.string/capitalize-all`. Przyjmuje ona również argument - łańcuch znaków, ale zwraca łańcuch, w którym wszystkie wyrazy mają pierwszą literę zmienioną na wielką.

```Clojure
(clojure.string/capitalize-all "hello world")
```

Output: "Hello World"

Możemy również wykorzystać funkcję `clojure.string/upper-case` do zamiany wszystkich liter na duże.

```Clojure
(clojure.string/upper-case "hello world")
```

Output: "HELLO WORLD"

## Głębsze zagłębienie

Funkcje `capitalize` i `capitalize-all` działają na podstawie obiektu `java.lang.String`, dlatego są w stanie obsłużyć również polskie znaki diakrytyczne.

Clojure oferuje również możliwość tworzenia własnych funkcji do kapitalizacji, wykorzystując na przykład pętlę `for` wraz z funkcją `clojure.string/upper-case`. Przykładowa implementacja wyglądałaby tak:

```Clojure
(defn capitalize-custom [string]
  (apply str (for [word (clojure.string/split string #"\s")]
               (str (clojure.string/upper-case (first word)) (substitute "/". "-" (rest word))))))

(capitalize-custom "hello world")
```

Output: "Hello World"

## Zobacz także

- Dokumentacja Clojure: https://clojure.org/
- Funkcjonalne programowanie w Clojure (artykuł w języku polskim): https://devstyle.pl/funkcyjnie-w-clojure-map-reduce/
- Poradnik użytkownika Clojure: https://clojure.org/guides/getting_started