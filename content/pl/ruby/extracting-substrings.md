---
title:                "Ruby: Wydobywanie podciągów"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyodrębnienie podciągów jest niezwykle przydatną umiejętnością w programowaniu, szczególnie w języku Ruby. Dzięki tej funkcji możemy łatwo manipulować i przetwarzać tekst, co często jest niezbędne w wielu projektach. W tym wpisie dowiesz się, dlaczego warto poznać tę funkcję i jak jej używać.

## Jak to zrobić

Ruby ma wiele metod pozwalających na wyodrębnianie podciągów, ale skupimy się na trzech najważniejszych: `slice`, `substring` i `split`. Każda z tych metod działa w nieco inny sposób, ale wszystkie pozwolą nam na wyciągnięcie określonych fragmentów tekstu.

Przykładowy kod i wynik dla każdej metody zostały przedstawione w poniższych blokach przy użyciu składni Markdown i Ruby.

```Ruby
# slice
str = "To jest przykładowy tekst"
puts str.slice(3, 9) # wypisze "jest przykł"

# substring
str = "To jest przykładowy tekst"
puts str.substring(9, 6) # wypisze "przykł"

# split
str = "To,jest,przykładowy,tekst"
puts str.split(",") # wypisze ["To", "jest", "przykładowy", "tekst"]
```

## Głębsze zanurzenie

Metody wykorzystywane do wyodrębniania podciągów mogą być również używane w połączeniu z innymi funkcjami, takimi jak `gsub` czy `chop`. Dokładniejsze informacje na temat tych funkcji można znaleźć w dokumentacji języka Ruby.

Pamiętaj również o wykorzystaniu metody `length` w celu sprawdzenia długości tekstu. Dzięki temu unikniesz błędów wynikających z próby wyodrębnienia fragmentu tekstu o długości większej niż cały tekst.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyodrębnianiu podciągów w Ruby, polecamy zapoznać się z poniższymi linkami:

- Dokumentacja Ruby: http://ruby-doc.org/core-2.6/String.html#method-i-slice
- Przewodnik dla początkujących: https://ruby-doc.com/docs/ProgrammingRuby/
- Opis metod wyodrębniania podciągów: https://www.geeksforgeeks.org/ruby-string-slice-method-with-examples/
- Przykładowe zadania dla praktyki: https://www.w3resource.com/ruby-exercises/string/ruby-string-exercise-9.php

Teraz jesteś gotowy, by spróbować wyodrębniania podciągów w praktyce. Baw się dobrze!