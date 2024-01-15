---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Ruby: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego 

Oto dlaczego warto uczyć się używać funkcji wyszukiwania i zamiany tekstu w Ruby: może to znacznie usprawnić naszą pracę przy przetwarzaniu i edycji danych. Zamiast ręcznie wykonywać mnóstwo operacji, możemy po prostu napisać kilka linijek kodu, które wykonają za nas to zadanie.

## Jak używać

Do wyszukiwania i zamiany tekstu w Ruby możemy użyć metody `gsub`, która działa na łańcuchach znaków. Przyjmuje ona dwa argumenty: pierwszy to wyrażenie regularne, które chcemy znaleźć, a drugi to łańcuch znaków, którym chcemy je zamienić. Na przykład:
```Ruby
text = "To jest przykładowy tekst, który chcemy zmodyfikować."

puts text.gsub("przykładowy", "nowy")
# Output: To jest nowy tekst, który chcemy zmodyfikować.
```
Możemy także użyć wyrażeń regularnych, aby jeszcze dokładniej wyszukać interesujący nas tekst. Przykładowo, jeśli chcemy zamienić wszystkie liczby w tekście na słowo "liczba", możemy to zrobić używając wyrażenia regularnego `\d+`, które oznacza jeden lub więcej cyfr. Zobaczmy to na przykładzie:
```Ruby
text = "W tym zdaniu jest 1234567890 cyfr."

puts text.gsub(/\d+/, "liczba")
# Output: W tym zdaniu jest liczba cyfr.
```

## Wnikliwa analiza

Funkcja `gsub` jest jednym z wielu sposobów na wyszukiwanie i zamianę tekstu w Ruby. Istnieją też inne metody, takie jak `sub` czy `tr`, które mogą być przydatne w różnych sytuacjach. Oprócz tego, wyrażenia regularne mogą być bardzo skomplikowane i wymagać trochę nauki, ale potrafią znacznie ułatwić i usprawnić pracę z tekstem.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyrażeniach regularnych i innych funkcjach Ruby do manipulacji tekstem, zapoznaj się z poniższymi linkami:

- [Dokumentacja Ruby dla metody `gsub`](https://ruby-doc.org/core-3.0.2/String.html#method-i-gsub)
- [Przewodnik po wyrażeniach regularnych w Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Artykuł o metodach String w Ruby](https://www.freecodecamp.org/news/ruby-string-methods/)
- [Kurs Ruby na platformie Codecademy](https://www.codecademy.com/learn/learn-ruby)