---
title:                "Zamiana liter w łańcuchu na wielkie"
html_title:           "Ruby: Zamiana liter w łańcuchu na wielkie"
simple_title:         "Zamiana liter w łańcuchu na wielkie"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Capitalizacja ciągu oznacza zamianę pierwszego znaku danego ciągu na wielką literę. Programiści często decydują się na tę funkcję, aby poprawić czytelność i formatowanie tekstu.

## Jak to zrobić:
Za pomocą metody `.capitalize()` w Ruby, zdolnej do kapitalizacji ciągu. Zobacz poniżej:
```Ruby
    zdanie = "witaj, świecie!"
    puts zdanie.capitalize()
```
W wyniku otrzymamy:
```
    "Witaj, świecie!"
```

## Deep Dive
Capitalizacja ciągu jest koncepcją istniejącą od czasów starych maszyn do pisania. W Ruby, tylko pierwszy znak ciągu jest zmieniany na wielką literę, pozostałe litery są zmieniane na małe litery. 

Alternatywą jest użycie metody `.titleize()` z Rails, która zamienia pierwszą literę każdego słowa na wielką literę.

Szczegółem implementacyjnym capitalizacji w Ruby jest to, że oryginalny ciąg nie jest modyfikowany, zamiast tego tworzona jest nowa kopia. 

```Ruby
    zdanie = "witaj, ŚWIECIE!"
    puts zdanie.capitalize()
```

## Zobacz także
 - [Ruby String capitalize method](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize)
 - [Ruby String capitalize explanation](https://www.geeksforgeeks.org/ruby-string-capitalize-method/).
 - [Alternative to capitalize](https://apidock.com/rails/String/titleize)