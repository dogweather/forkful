---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Ruby: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwertowanie ciągu znaków na małe litery jest jedną z podstawowych operacji w programowaniu. Polega ono na zamianie wszystkich liter w ciągu na ich małe odpowiedniki. Programiści wykonują tę operację przede wszystkim w celu ujednolicenia danych oraz ułatwienia ich porównywania.

## Jak to zrobić:

```Ruby
string = "ZWIERZĄTKA"
puts string.downcase 
# => "zwierzątka"
```

W powyższym przykładzie wykorzystujemy metodę `downcase` na stringu "ZWIERZĄTKA". Metoda ta zwraca nowy ciąg znaków z wszystkimi literami zmienionymi na małe. Możemy również wykorzystać operator `<<` aby zmienić oryginalny ciąg bez tworzenia nowego obiektu.

```Ruby
string = "MYSZKA"
string.downcase!
puts string
# => "myszka"
```

## Głębsza Analiza:

Konwertowanie ciągu znaków na małe litery jest ważną częścią wielu działań w programowaniu. Nie zawsze taka operacja była dostępna w językach programowania. W starszych językach typu C, konieczne było ręczne przekształcenie każdej litery w pętli. Współczesne języki programowania, takie jak Ruby, udostępniają gotową metodę, która znacznie ułatwia ten proces.

Alternatywami dla metody `downcase` są `upcase` (konwertuje na duże litery) oraz `swapcase` (zamienia wielkość liter na przeciwną). Można również użyć metody `capitalize` aby zamienić tylko pierwszą literę na dużą.

Wewnętrzna implementacja metody `downcase` może się różnić w zależności od języka programowania. W przypadku Rubiego, wykorzystywane jest standardowe tłumaczenie mapujące każdy znak na jego mały odpowiednik.

## Zobacz także:

- [Ruby Docs on String#downcase](https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase)
- [Alternatives to downcase in Ruby](https://stackoverflow.com/questions/11873534/alternative-to-djoncase-in-ruby)