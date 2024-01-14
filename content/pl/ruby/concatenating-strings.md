---
title:                "Ruby: Łączenie ciągów znaków"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Komponowanie ciągów znaków jest powszechną praktyką w programowaniu, która pozwala na łączenie różnych wartości lub tekstów w jedno. Jest to przydatne w celu tworzenia bardziej czytelnych i dynamicznych aplikacji.

## Jak to zrobić

```Ruby
# Pierwszy sposób: użycie operatora "+" do łączenia dwóch ciągów znaków
ciag1 = "Witaj "
ciag2 = "świecie!"

puts ciag1 + ciag2

# Wynik:
# Witaj świecie!

# Drugi sposób: użycie metody "concat" do łączenia kilku ciągów 
imie = "Kamil"
nazwisko = " Nowak"
wiek = "25"

imie.concat(nazwisko, " ma ", wiek, " lat.")

puts imie

# Wynik:
# Kamil Nowak ma 25 lat.
```

## Głębszy wgląd

Ponieważ ciągi znaków w Ruby są obiektami, można wykonywać na nich różne metody, takie jak "concat" czy też "<<" do łączenia innych ciągów lub wartości. Ponadto, przy użyciu interpolacji ciągów, można wygodnie łączyć zmienne i wyrażenia w jednym ciągu znaków.

## Zobacz także 
- [Oficjalna dokumentacja Ruby o łączeniu ciągów](https://ruby-doc.org/core-2.7.1/String.html#method-i-2B)
- [Przykładowe zadania związane z łączeniem ciągów w Ruby](https://www.codewars.com/kata/search/ruby?q=concatenation)