---
title:    "Ruby: Wyszukiwanie i zamiana tekstu"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Wpisz tutaj krótkie wyjaśnienie, dlaczego ktoś mógłby być zainteresowany przeszukiwaniem i zastępowaniem tekstu za pomocą języka Ruby.

## Jak To Zrobić

Przeszukiwanie i zastępowanie tekstu to powszechne zadanie w procesie tworzenia oprogramowania. Dzięki językowi Ruby możemy to zadanie wykonać szybko i łatwo. Poniżej znajdziesz przykłady kodu oraz wyników dla różnych przypadków.

```Ruby
# Przykład 1: Zamiana wszystkich małych liter na wielkie

tekst = "witaj świecie!"
puts tekst.upcase

# Wynik: WITAJ ŚWIECIE!

# Przykład 2: Zastąpienie określonego słowa innym

tekst = "Kocham Ruby!"
puts tekst.gsub("Ruby", "programowanie")

# Wynik: Kocham programowanie!

# Przykład 3: Usunięcie znaków specjalnych

tekst = "~~Ruby~~"
puts tekst.delete("~")

# Wynik: Ruby
```

## Wnikliwe Spostrzeżenia

Przeszukiwanie i zastępowanie tekstu to ważny element procesu tworzenia oprogramowania. Dzięki językowi Ruby możemy wykonać te zadania w prosty i skuteczny sposób. Jednak warto pamiętać o różnicach między użytymi metodami, takimi jak "gsub" i "delete". Głębsze zrozumienie tych różnic pozwoli Ci lepiej dostosować swoje rozwiązanie do konkretnego przypadku.

## Zobacz także

- [Dokumentacja Ruby dla metod String](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby Tutorial: Manipulowanie Tekstem](https://www.rubyguides.com/2019/05/ruby-string-methods/)
- [Ruby Monk: Zastępowanie Tekstu w Ruby](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-more-strings/lessons/118-substitution-in-ruby)