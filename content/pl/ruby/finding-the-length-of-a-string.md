---
title:                "Ruby: Znajdowanie długości ciągu znaków"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków jest jedną z podstawowych operacji w programowaniu. W tym artykule dowiesz się, dlaczego znajdowanie długości ciągu jest ważne oraz jak to zrobić w języku Ruby.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Ruby, możesz użyć metody `length` lub `size`. Przykładowy kod wyglądałby następująco:

```Ruby
string = "Hello World" # przykładowy ciąg znaków
puts "Długość ciągu: #{string.length}" # wyświetli 11
```

Jeśli chcesz znaleźć długość ciągu złożonego z wielu wierszy, możesz użyć metody `each_line` oraz zastosować metodę `length` na każdym wierszu. Przykład:

```Ruby
multiline_string = "Linia 1\nLinia 2\nLinia 3" # ciąg wielu wierszy
multiline_string.each_line do |line|
  puts "Długość wiersza #{line}: #{line.length}"
end

# wynik:
# Długość wiersza Linia 1: 6
# Długość wiersza Linia 2: 6
# Długość wiersza Linia 3: 6
```

Możesz również użyć metody `bytesize`, która zwróci długość ciągu w bajtach.

```Ruby
string = "Cześć!"
puts "Długość ciągu w bajtach: #{string.bytesize}"

# wynik: 7
```

## Deep Dive

W języku Ruby ciągi znaków są przechowywane jako obiekty klasy `String`. Metody `length` i `size` wykorzystują wewnętrzny atrybut `length` tego obiektu, aby zwrócić długość ciągu. Metoda `bytesize` natomiast zlicza ilość bajtów wewnątrz ciągu.

Warto też zaznaczyć, że metoda `length` może nie zawsze być dokładna dla ciągów zawierających niestandardowe znaki lub emotikony, ponieważ tłumacząc na bity może dojść do przekształceń.

## Zobacz także

- Dokumentacja Ruby o metodach `length` i `size`: https://ruby-doc.org/core-2.7.4/String.html#method-i-length
- Artykuł na temat długości ciągu znaków w Ruby: https://www.rubyguides.com/2019/06/ruby-string-length/