---
title:                "Znajdowanie i zamienianie tekstu"
html_title:           "Elixir: Znajdowanie i zamienianie tekstu"
simple_title:         "Znajdowanie i zamienianie tekstu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

"Szukaj i zamieniaj" (ang. search and replace) to proces polegający na wyszukiwaniu określonych fragmentów tekstu i zastępowaniu ich innymi. Jest to często stosowane przez programistów do szybkiej zmiany wielu wystąpień danej frazy na inną, bez konieczności ręcznego edytowania każdego z nich.

## Jak to zrobić:

Możemy użyć funkcji ```Elixir String.replace/3``` do wyszukania i zamiany tekstu w łańcuchu znaków. Na przykład, 
```
Elixir String.replace("Witaj świecie!", "świecie", "śliwka")
```
zwróci ```"Witaj śliwka!"```. Można również używać wyrażeń regularnych do jeszcze bardziej zaawansowanych wyszukiwań i zamian.

## Głębsza analiza:

Technika szukaj i zamieniaj została opracowana w latach 70. XX wieku i od tego czasu jest stosowana w różnych językach programowania. W Elixir możemy również użyć funkcji ```Elixir String.replace/4``` do wykonania wyszukiwania i zamiany jedynie na określonej liczbie wystąpień w tekście. Alternatywnym sposobem jest użycie biblioteki ```Elixir Regex``` do użycia wyrażeń regularnych.

## Zobacz też:

- Dokumentacja Elixir dla funkcji ```String.replace/3```
- Dokumentacja Elixir dla funkcji ```Regex.run/2```