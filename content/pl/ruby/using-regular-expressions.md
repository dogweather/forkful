---
title:    "Ruby: Używanie wyrażeń regularnych"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego korzystać z wyrażeń regularnych?

Wyrażenia regularne są niezwykle przydatne w programowaniu w języku Ruby. Pozwalają one na wyrażenie i wyszukiwanie wzorców w tekście, co jest niezbędne w wielu zastosowaniach, takich jak przetwarzanie danych i walidacja formularzy. Korzystanie z wyrażeń regularnych może znacznie ułatwić pracę i przyspieszyć proces tworzenia oprogramowania.

## Jak używać wyrażeń regularnych w Ruby?

Aby używać wyrażeń regularnych w Ruby, należy wykorzystać metodę `match`. Można ją wywołać na dowolnym ciągu znaków i przekazać jako argument wyrażenie regularne w postaci obiektu `Regexp`. Poniżej przedstawiamy przykładowe wyrażenie regularne, które znajduje wszystkie liczby w tekście:

```ruby
text = "Lorem ipsum dolor sit 123 amet, consectetur adipiscing elit."
matches = text.match(/(\d+)/)
puts matches[1]
```

Output: `123`

Możemy również użyć wyrażenia regularnego w warunku `if` lub `unless`, aby sprawdzić, czy dany tekst pasuje do wzorca. Na przykład:

```ruby
text = "Lorem ipsum dolor sit 123 amet, consectetur adipiscing elit."
if text.match?(/(\d+)/)
  puts "Tekst zawiera liczbę."
end
```

Output: `Tekst zawiera liczbę.`

## Głębsza analiza wyrażeń regularnych

Wyrażenia regularne w Ruby posiadają wiele opcji i metod, co czyni je bardzo potężnym narzędziem. Poniżej przedstawiamy kilka przydatnych informacji o wyrażeniach regularnych.

### Opcje wyrażeń regularnych

Wyrażenia regularne mogą mieć opcje, które zmieniają zachowanie dopasowywania. Opcje te są przekazywane jako drugi argument do metody `match`. Na przykład, jeśli chcemy dopasować tekst bez uwzględniania wielkości liter, możemy użyć opcji `i`:

```ruby
text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
matches = text.match(/(lorem)/i)
puts matches[1]
```

Output: `Lorem`

Innym przykładem jest opcja `m`, która powoduje, że poszczególne linie tekstu są traktowane oddzielnie, a nie cały tekst jako jeden ciąg znaków.

### Metody dla wyrażeń regularnych

Obiekty `Regexp` posiadają wiele przydatnych metod, które ułatwiają pracę z wyrażeniami regularnymi. Poniżej przedstawiamy kilka z nich:

- `source` – zwraca wyrażenie regularne w postaci ciągu znaków
- `scan` – zwraca wszystkie dopasowania do danego wyrażenia regularnego jako tablicę
- `sub` – zamienia pierwsze dopasowanie wyrażenia regularnego z podanym ciągiem znaków
- `gsub` – zamienia wszystkie dopasowania wyrażenia regularnego z podanym ciągiem znaków

Istnieje wiele innych metod, które można wykorzystać w zależności od potrzeb.

## Zobacz również

- [Ruby: Regular Expressions](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)