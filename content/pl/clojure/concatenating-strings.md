---
title:                "Clojure: Łączenie ciągów znaków"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Połączenie ciągów znaków, zwane także konkatenacją, jest powszechną czynnością w programowaniu. Jest niezbędne, jeśli chcemy łączyć różne ciągi znaków w jeden i tworzyć dynamiczne teksty. W tym artykule dowiesz się, dlaczego warto poznać ten podstawowy koncept oraz jak wykorzystać go w języku Clojure.

## Jak to zrobić?

Pierwszym krokiem do łączenia ciągów znaków jest użycie funkcji `str`. Przyjmuje ona dowolną liczbę argumentów i łączy je w jeden ciąg znaków. Na przykład:

```Clojure
(str "Cześć, " "to jest " "blog " "o Clojure!")
```

Output: `Cześć, to jest blog o Clojure!`

Możemy również użyć znaku `+`, aby połączyć dwa ciągi znaków. Na przykład:

```Clojure
("Hello " + "world!")
```

Output: `Hello world!`

Jeśli chcemy połączyć więcej niż dwa ciągi, musimy użyć funkcji `str` lub `reduce`.

## Pogłębione spojrzenie

Podczas konkatenacji ciągów warto pamiętać o wydajności. Używanie funkcji `str` jest szybsze niż używanie `+`, ponieważ unika konwersji typów wewnętrznie. Dlatego też zaleca się stosowanie `str`, jeśli łączymy więcej niż dwa ciągi.

Warto również zwrócić uwagę na kolejność argumentów w funkcji `str`. Argumenty zostaną połączone w tej samej kolejności, w jakiej je podaliśmy. Możemy jednak zmienić kolejność, używając funkcji `reverse` lub `map`.

## Zobacz także

* Oficjalna dokumentacja Clojure: https://clojure.org/api/cheatsheet
* Przykłady konkatenacji w Clojure: https://www.baeldung.com/clojure/string-concatenation