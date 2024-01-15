---
title:                "Pisanie do standardowego błędu"
html_title:           "Elixir: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest nieodzownym elementem w wielu językach programowania, w tym również w Elixir. Jest to ważny aspekt w procesie debugowania i znajomość jego zastosowań może pomóc w rozwiązywaniu problemów w aplikacjach.

## Jak to zrobić

Aby napisać do standardowego błędu w Elixir, użyjemy funkcji `IO.write(:stderr, "Tekst")`, gdzie `"Tekst"` jest wiadomością, którą chcemy wyświetlić. Możemy również użyć funkcji `IO.puts(:stderr, "Tekst")`, która dodaje znak nowej linii na końcu wiadomości.

```
Elixir

IO.write(:stderr, "Błąd!") IO.puts(:stderr, "To jest tekst do wyświetlenia w błędzie.")
```

Będzie to wyglądać następująco w konsoli:

```
Błąd!
To jest tekst do wyświetlenia w błędzie.
```

## Deep Dive

W Elixir istnieje również możliwość wyświetlania błędów wraz z informacjami o stosie wywołań. Aby to zrobić, używamy funkcji `IO.inspect(:stderr, message, opts)` z opcją `:show_stacktrace` ustawioną na true. Może to być przydatne w przypadku, gdy potrzebujemy bardziej szczegółowych informacji o błędzie.

```
Elixir

opts = [show_stacktrace: true] IO.inspect(:stderr, "Błąd!", opts)
```
Będzie to wyglądać podobnie jak wcześniej, ale z dodatkowymi informacjami o stosie wywołań:

```
Błąd!
stacktrace:
[
{Moduł, funkcja, argumenty, plik: wiersz},
{Moduł2, funkcja2, argumenty2, plik2: wiersz2}
...
]
```

Możemy również użyć funkcji `IO.inspect(:stderr, message, opts)` z opcją `:label` ustawioną na `"Błąd"` lub inną wiadomość, aby uzyskać bardziej czytelną informację o błędzie.

## Zobacz także

- [Dokumentacja Elixir o obsłudze błędów](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%3C%3C%3E%2F1)
- [Poradnik: Debugowanie w Elixir](https://blog.plataformatec.com.br/2016/04/debugging-elixir-processes/)