---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Łączenie łańcuchów znaków w Ruby: Przewodnik krok po kroku

## Co to jest i dlaczego?

Łączenie łańcuchów znaków, znane również jako konkatenacja, polega na łączeniu dwóch lub więcej ciągów znaków w jeden. Programiści robią to do manipulacji danymi tekstowymi i tworzą efektywne interfejsy użytkownika.

## Jak to zrobić:

Jest wiele sposobów łączenia łańcuchów znaków w Ruby. Pokażę kilka przykładów.

``` Ruby
# Metoda 1: Użycie operatora '+'
imie = 'Adam'
powitanie = 'Cześć, ' + imie
puts powitanie # Wydrukuj 'Cześć, Adam'

# Metoda 2: Użycie operatora '<<'
powitanie = 'Cześć, '
powitanie << 'Adam'
puts powitanie # Wydrukuj 'Cześć, Adam'
```

## Głębsze zrozumienie

Kiedy łączymy łańcuchy znaków, Ruby tworzy nowy obiekt. Z punktu widzenia wydajności, operator `<<` jest lepszy, ponieważ zmienia oryginalny łańcuch bez tworzenia nowego obiektu. 

Historycznie, konkatenacja była w niektórych językach programowania trudna do osiągnięcia. Ruby jednak od początku swojego istnienia ułatwia to zadanie.

Istnieją również metody alternatywne, takie jak `concat()` lub `join()`. Wymagają one jednak większej wiedzy i umiejętności.

## Zobacz także:

1. Dokumentacja Ruby dla łańcuchów znaków: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)