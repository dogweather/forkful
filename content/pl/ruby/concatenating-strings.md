---
title:                "Łączenie ciągów tekstowych"
html_title:           "Ruby: Łączenie ciągów tekstowych"
simple_title:         "Łączenie ciągów tekstowych"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czemu ktokolwiek chciałby się zająć łączeniem ciągów znaków? Jest to często używany w programowaniu sposób na tworzenie dynamicznych komunikatów, wygenerowanie unikalnych identyfikatorów lub manipulowanie tekstem w zależności od różnych warunków.

## Jak to zrobić

```Ruby
# łączenie stałych ciągów znaków
puts "Hello " + " world!" 
# output: Hello world!

# łączenie zmiennych ciągów znaków
name = "John"
puts "Hello, " + name
# output: Hello, John

# łączenie różnych typów danych
number = 3
puts "I have " + number.to_s + " apples."
# output: I have 3 apples.
```

## Głębszy wgląd

W języku Ruby, łączenie ciągów znaków odbywa się za pomocą operatora "+" lub metody "concat". Jednakże użycie operatora jest zalecane ze względu na wydajność. Dodatkowo, do łączenia ciągów znaków wykorzystywana jest metoda "<<", jednak jest to mniej wydajne niż użycie operatora "+".

## Zobacz też

- Dokumentacja Ruby: https://ruby-doc.org/core-3.0.1/String.html#method-i-2B
- Poradnik dla początkujących w Ruby: https://www.ruby-lang.org/pl/documentation/quickstart/