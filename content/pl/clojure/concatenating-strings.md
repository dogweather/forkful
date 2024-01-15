---
title:                "Łączenie ciągów znaków"
html_title:           "Clojure: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenacja jest procesem łączenia wielu ciągów znaków w jeden większy ciąg. Jest to często spotykana operacja w programowaniu, która jest używana do tworzenia czytelnych i dynamicznych wyjść dla użytkownika. W Clojure, możemy łatwo łączyć ciągi znaków w bardziej złożone struktury danych za pomocą funkcji concatenation.

## Jak to zrobić

Możemy użyć funkcji `str` w Clojure, aby połączyć dwa lub więcej ciągów znaków w jeden. Na przykład, jeśli chcielibyśmy połączyć "Hello" i "World", nasz kod wyglądałby następująco:

```Clojure
(str "Hello" "World")
```

Jego wynikiem będzie "HelloWorld". Możemy również użyć zmiennej do przechowywania ciągów znaków i połączyć je później za pomocą funkcji `str`. Na przykład:

```Clojure
(def str1 "Hello")
(def str2 "World")
(str str1 str2)
```
Wynikiem będzie również "HelloWorld".

Możemy również użyć operatora `str` do łączenia ciągów znaków w mapach i listach. Na przykład, jeśli chcemy łączyć wartości z mapy, nasz kod może wyglądać tak:

```Clojure
(def map1 {:name "John" :age "30"})
(str (str (:name map1) " is " (:age map1) " years old."))
```
Wynikiem będzie "John is 30 years old."

## Głębsze zagłębianie

Funkcja `str` w Clojure jest bardzo elastyczna, ponieważ pozwala nam łączyć różne typy danych, takie jak liczby, znaki i obiekty. Możemy również użyć operatora `str` do interpolacji, co pozwala nam wstawiać zmienne do ciągów znaków. Na przykład:

```Clojure
(def name "John")
(str "My name is ${name}.")
```
Wynikiem będzie "My name is John." 

Warto również wspomnieć o funkcji `join`, która jest również użyteczna do łączenia ciągów znaków. Jednak w przeciwieństwie do `str`, `join` działa na kolekcjach, takich jak listy i mapy, i wymaga podania separatora. Na przykład, jeśli chcemy połączyć elementy w liście z separatorem "-":

```Clojure
(def list1 ["hello" "world"])
(join "-" list1)
```
Wynikiem będzie "hello-world".

## Zobacz również

- Dokumentacja Clojure: https://clojure.org/
- Oficjalna strona Clojure: https://clojure.org/
- Poradnik dla początkujących w Clojure: https://www.braveclojure.com/