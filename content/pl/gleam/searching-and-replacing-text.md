---
title:                "Gleam: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, gdy piszemy kod, możemy popełnić błąd i chcieć zmienić pewne elementy tekstu bez konieczności przepisywania go od początku. W takim przypadku narzędzie do wyszukiwania i zastępowania tekstu jest niezwykle przydatne. Pozwala ono szybko i bezproblemowo dokonać zmian w kodzie, co może zaoszczędzić nam czas i wysiłek.

## Jak to zrobić

W Gleam istnieje wbudowana funkcja do wyszukiwania i zastępowania tekstu, a jej użycie jest bardzo proste. Oto przykładowy kod:

```Gleam
a = "Hello, world!"
b = String.replace(a, "world", "Gleam")
```

Kod ten utworzy nowy string o wartości "Hello, Gleam!", zastępując słowo "world" na "Gleam". Zobaczmy, jak wygląda to w praktyce:

```Gleam
a = "This is a sentence."
b = String.replace(a, "sentence", "paragraph")
/* b teraz zawiera "This is a paragraph." */
```

Łatwo zauważyć, że funkcja ta jest bardzo przydatna, gdy chcemy zmienić pojedyncze słowo lub wyrażenie w większym tekście.

## Głębsza analiza

W Gleam, funkcja `String.replace` działa na podstawie wzorca, który ma zostać zastąpiony oraz tekstu, w którym ma nastąpić zamiana. Możemy również podać trzeci argument, aby określić maksymalną liczbę zastąpień. Możemy również użyć funkcji `String.replace_all`, która zastąpi wszystkie wystąpienia wzorca w tekście.

Używanie funkcji do wyszukiwania i zastępowania tekstu może również pomóc nam w refaktoryzacji kodu. Możemy użyć jej do zmiany nazw zmiennych lub funkcji w całym projekcie jednym poleceniem.

## Zobacz także

- Dokumentacja Gleam dla funkcji `String.replace`: https://gleam.run/doc/current/stdlib/String.html#replace
- Przykłady wykorzystania funkcji `String.replace`: https://medium.com/@gleam_blogger/5-useful-gleam-string-functions-c33636a65477
- Przykład refaktoryzacji kodu z użyciem funkcji `String.replace`: https://dev.to/gauranshkumar/refactoring-with-applicative-python-3eie