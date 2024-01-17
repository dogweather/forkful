---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Ruby: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Długość łańcucha znaków to nic innego jak liczba znaków znajdujących się w danym łańcuchu. Programiści często potrzebują tego podoałbyć, aby sprawdzić, czy użytkownik wpisał odpowiednią ilość znaków w polu formularza lub w celu przetwarzania tekstu w swoim programie.

## Jak to zrobić:

```Ruby
str = "Hello world!" # nasz łańcuch znaków
puts str.length # wypisze 12
```

```Ruby
str2 = "" # pusty łańcuch znaków
puts str2.length # wypisze 0
```

## Głębsze werte:

Długość łańcucha znaków jest obliczana przez zliczenie liczby znaków w łańcuchu. W przeszłości programiści często musieli sami napisać funkcję do liczenia długości łańcucha, ale dzięki postępowi technologicznemu, większość języków programowania, w tym Ruby, ma wbudowaną funkcję length() lub size(), które ułatwiają to zadanie.

## Zobacz też:

Jeśli chcesz dowiedzieć się więcej o manipulacji łańcuchami znaków w Rubym, przeczytaj artykuł [Working with Strings in Ruby](https://www.rubyguides.com/ruby-string-methods/).

Jeśli interesują Cię alternatywne sposoby liczenia długości łańcucha w Rubym, przeczytaj [Ruby docs on strings](https://ruby-doc.org/core-2.7.2/String.html) lub [Stack Overflow thread](https://stackoverflow.com/questions/1753216/how-do-i-get-the-size-of-a-ruby-string).