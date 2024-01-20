---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Znalezienie długości stringa to jedno z podstawowych zadań w programowaniu, polega na określeniu liczby znaków w łańcuchu. Jako programiści robimy to, aby kontrolować i przetwarzać dane na potrzeby różnych funkcji i algorytmów.

## Jak to zrobić:

```Ruby
napis = "Witaj, Ruby!"
puts napis.length
```
**Wyjście:**

```Ruby
13
```
Wykorzystując metodę `.length`, możemy łatwo znaleźć długość stringa w Ruby. 

Innym przykładem jest użycie metody `.size`.

```Ruby
napis = "Witaj, Ruby!"
puts napis.size
```
**Wyjście:**

```Ruby
13
```

## Głębsze spojrzenie

Znalezienie długości łańcucha to jedno z podstawowych zadań, które programiści wykonują od początków powstania programowania. Metoda `.length` w Ruby działa poprzez iterację przez stringa i zliczanie wszystkich znaków,  niezależnie od tego, czy jest to spacja, litera, liczba czy jakikolwiek inny symbol.

Choć `.length` i `.size` są najczęściej używanymi metodami w Ruby do znalezienia długości stringa, jest jeszcze metoda `.bytesize`, która zwraca liczbę bajtów, a nie znaków, co jest użyteczne, kiedy pracujemy z kodowaniem wielobajtowym.

```Ruby
napis = "ćma"
puts napis.length
puts napis.bytesize
```
**Wyjście:**

```Ruby
3
6
```

## Zobacz także:

- [Dokumentacja metody length w Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-length)
- [Dokumentacja metody size w Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-size)
- [Dokumentacja metody bytesize w Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-bytesize)