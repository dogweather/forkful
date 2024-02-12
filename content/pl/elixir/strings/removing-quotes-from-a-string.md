---
title:                "Usuwanie cudzysłowów z ciągu znaków"
aliases:
- /pl/elixir/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:32.856242-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów z ciągu znaków oznacza pozbycie się tych dodatkowych wrapperów, aby uzyskać czysty tekst wewnątrz. Programiści robią to, aby oczyścić dane wejściowe, uniknąć błędów i przygotować dane do przetworzenia, gdzie cudzysłowy są utrudnieniami, a nie funkcjami.

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
