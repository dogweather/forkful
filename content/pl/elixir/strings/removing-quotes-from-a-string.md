---
date: 2024-01-26 03:39:32.856242-07:00
description: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w oznacza pozbycie\
  \ si\u0119 tych dodatkowych wrapper\xF3w, aby uzyska\u0107 czysty tekst wewn\u0105\
  trz. Programi\u015Bci robi\u0105 to, aby oczy\u015Bci\u0107\u2026"
lastmod: '2024-03-13T22:44:35.028405-06:00'
model: gpt-4-0125-preview
summary: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w oznacza pozbycie si\u0119\
  \ tych dodatkowych wrapper\xF3w, aby uzyska\u0107 czysty tekst wewn\u0105trz."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Elixir nie ma wbudowanej funkcji 'usuń cudzysłowy', ale stworzenie własnej funkcji za pomocą dopasowania wzorca lub funkcji `String` to pestka. Oto kilka fragmentów:

```elixir
# Przy użyciu dopasowania wzorca
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Przykładowe użycie
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# Przy użyciu String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Przykładowe użycie
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

Wynik dla obu metod będzie:
```
"Hello, World!"
```

## Pogłębiona analiza
W przeszłości, cudzysłowy w ciągach znaków były polem minowym — źle je obsłuż i boom, błędy składni czy luki w zabezpieczeniach. W Elixirze, dopasowanie wzorca traktuje twoje ciągi jak klocki Lego, pozwalając wybrać i zbudować z precyzją. Jego solidny moduł `String` również jest przydatny, elastycznie eliminując cudzysłowy za pomocą funkcji `trim`. Alternatywy? Wyrażenia regularne mogą pozbyć się cudzysłowów, a zewnętrzne biblioteki mogą zaoferować dodatkową siłę ognia, jeśli potrzebujesz więcej niż podstawowe usuwanie.

## Zobacz również
Zagłęb się głębiej dzięki tym zasobom:
- [Moduł String w Elixirze](https://hexdocs.pm/elixir/String.html)
- [Dowiedz się więcej o dopasowaniu wzorca w Elixirze](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Wyrażenia regularne w Elixirze (moduł Regex)](https://hexdocs.pm/elixir/Regex.html)
