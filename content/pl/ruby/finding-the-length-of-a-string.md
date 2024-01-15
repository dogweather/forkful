---
title:                "Znajdowanie długości łańcucha znaków"
html_title:           "Ruby: Znajdowanie długości łańcucha znaków"
simple_title:         "Znajdowanie długości łańcucha znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości ciągu znaków jest niezbędne w wielu programach, szczególnie do sprawdzania poprawności wprowadzanych danych i ich przetwarzania. W języku Ruby istnieje kilka różnych sposobów na znalezienie długości stringa, które omówimy w tym artykule.

## Jak to zrobić

Po pierwsze, można użyć metody `length` na obiekcie stringa, która zwróci liczbę znaków w ciągu. Przykładowy kod wyglądałby tak:

```Ruby
str = "To jest przykładowy ciąg znaków."
puts str.length
```

**Wynik:**

`33`

Kolejną możliwością jest użycie metody `size`, która działa w podobny sposób do `length`. Jednak w niektórych przypadkach może dać nieco odmienny wynik, ponieważ działa także na innego rodzaju obiektach.

```Ruby
str = "To jest kolejny przykład."
puts str.size
```

**Wynik:**

`25`

Można również wykorzystać metodę `bytesize`, która zwraca liczbę bajtów użytych do przechowywania danego ciągu. Jest to przydatne, gdy pracujemy z ciągami znaków w różnych kodowaniach.

```Ruby
str = "Tutaj jest użyty UTF-8"
puts str.bytesize
```

**Wynik:**

`21`

Ostatnią metodą, którą omówimy jest `count`, która pozwala na policzenie wystąpień określonego znaku lub ciągu znaków w danym stringu.

```Ruby
str = "Jestem programistą Ruby."
puts str.count("Ruby")
```

**Wynik:**

`1`

## Deep Dive

W języku Ruby stringi są traktowane jako tablice znaków, co oznacza, że można na nich wykonywać metody charakterystyczne dla tablic. Dlatego też można użyć metody `each_char`, aby w przejrzysty sposób wyświetlić każdy znak danego stringa.

```Ruby
str = "Programowanie jest super!"
str.each_char do |char|
  puts char
end
```

**Wynik:**

`P`

`r`

`o`

`g`

`r`

`a`

`m`

`o`

`w`

`a`

`n`

`i`

`e`

` `

`j`

`e`

`s`

`t`

` `

`s`

`u`

`p`

`e`

`r`

`!`

## Zobacz także

- [Dokumentacja Ruby](https://www.ruby-lang.org/pl/documentation/)
- [Kurs Ruby na Codecademy](https://www.codecademy.com/learn/learn-ruby)
- [Ruby dla początkujących – artykuł na Medium](https://medium.com/code-interview/ruby-dla-pocz%C4%85tkuj%C4%85cych-podsumowanie-bcb660c4c5a1)