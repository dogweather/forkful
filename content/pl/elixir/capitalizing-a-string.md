---
title:                "Zamiana ciągu na wielkie litery"
html_title:           "Elixir: Zamiana ciągu na wielkie litery"
simple_title:         "Zamiana ciągu na wielkie litery"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Capitalizacja łańcucha oznacza zmienienie pierwszej litery napisu na wielką. Programiści robią to, aby poprawić czytelność i formatowanie tekstu w aplikacjach i stronach internetowych.

## Jak to zrobić:
W Elixir, zastosuj funkcję `String.capitalize/2` do zmiany pierwszej litery napisu na wielką literę.
```Elixir
iolist = IO.iodata_to_binary('elixir jest mocny')
String.capitalize(iolist, :pl)
```
Przykładowe wyjście:
```Elixir
"Elixir jest mocny"
```

## Głębsze zrozumienie
- Kontekst historyczny: W starożytnych systemach informatycznych, takich jak systemy pracujące w trybie tekstowym, używano capitalizacji, aby zaznaczyć początki zdań, tytułów, nazw własnych itd.
- Alternatywy: W Elixir, można również użyć `String.upcase/1` aby zmienić wszystkie litery na wielkie, lub `String.downcase/1` aby zmienić wszystkie litery na małe.
- Detale implementacji: Funkcja `String.capitalize/2` działa na bajtach, nie znakach, co oznacza, że może nie działać poprawnie na ciągach zawierających znaki spoza standardowego zestawu ASCII. Użycie `iodata_to_binary` przed `capitalize` zapobiega temu problemowi.

## Zobacz też:
- Dokumentacja Elixir dla [String.capitalize](https://hexdocs.pm/elixir/String.html#capitalize/2)
- Elixir Forum: [Wątek](https://elixirforum.com/t/how-do-i-properly-capitalize-strings/3852) na temat capitalizacji napisów.