---
title:    "Ruby: Wyciąganie podciągów"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać wyciągania substrings w programowaniu Ruby?

Wyciąganie substrings jest niezwykle przydatną funkcją w programowaniu Ruby, która pozwala na wyodrębnianie określonych części tekstu z ciągów znaków. Może to być przydatne w wielu sytuacjach, np. podczas pracy z danymi wejściowymi lub podczas tworzenia aplikacji webowych. W tym artykule dowiesz się, dlaczego warto nauczyć się używać tej funkcji i jak zrobić to w prosty sposób.

## Jak wykorzystać wyciąganie substrings w programowaniu Ruby?

Aby użyć funkcji wyciągania substrings w języku Ruby, wystarczy użyć metody `slice` lub `[]` z odpowiednimi parametrami. Ważne jest również przypisanie wyniku do zmiennej lub wyświetlenie go w konsoli za pomocą metody `puts`.

Przykładowe użycie metody `slice`:

```
# Tworzymy zmienną tekstową
text = "Witaj w świecie Ruby!"

# Wyciągamy pierwszych 5 liter z ciągu znaków
puts text.slice(0, 5)
# Output: Witaj

# Wyciągamy ostatnie 8 liter z ciągu znaków
puts text.slice(-8, 8)
# Output: Ruby!
```

Przykładowe użycie metody `[]`:

```
# Tworzymy zmienną tekstową
text = "Coding to jest super!"

# Wyciągamy pierwsze 6 liter z ciągu znaków
puts text[0, 6]
# Output: Coding

# Wyciągamy fragment tekstu pomiędzy 11 a 16 znakiem
puts text[11...16]
# Output: jest
```

## Głębsze zagadnienia wyciągania substrings

Metody `slice` i `[]` pozwalają również na wykorzystanie wyrażeń regularnych w celu wyciągnięcia jeszcze bardziej precyzyjnych fragmentów tekstu. Można również użyć dodatkowych argumentów np. `downcase` lub `upcase` w celu zmiany wielkości liter w wynikowym ciągu.

```
# Wykorzystanie wyrażenia regularnego
text = "Kodowanie jest super!"

puts text.slice(/\w+/)
# Output: Kodowanie

# Użycie argumentów
puts text[1, 2].upcase
# Output: O D
```

Warto również zaznaczyć, że metoda `slice` pozwala na odnoszenie się do konkretnego znaku w ciągu znaków za pomocą jego indeksu. Natomiast `[]` daje większe możliwości dzięki wykorzystaniu różnych parametrów, np. wyrażeń regularnych. Warto więc przetestować obie metody i wybrać tę, która najlepiej pasuje do danego przypadku użycia.

## Zobacz także

- Dokumentacja Ruby o metodach `slice` i `[]`: https://ruby-doc.org/core-2.6.3/String.html#method-i-slice
- Przykładowe zadania z wyciągania substrings: https://www.codewars.com/kata/search/ruby?q=substring
- Kurs programowania w języku Ruby: https://www.codecademy.com/learn/learn-ruby