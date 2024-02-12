---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- pl/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:27.304585-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana pierwszej litery łańcucha na wielką, a reszty na małe litery, oznacza modyfikację tekstu w taki sposób, by pierwsza litera była wielka, a pozostała część łańcucha była pisana małymi literami. Jest to powszechne zadanie w przetwarzaniu tekstów, normalizacji wejścia użytkownika oraz formacie danych, aby zapewnić spójność lub spełnić określone kryteria formatowania.

## Jak to zrobić:

W Fish Shell, ciągi znaków mogą być manipulowane bezpośrednio za pomocą wbudowanych funkcji, bez potrzeby używania zewnętrznych narzędzi czy bibliotek. Aby uczynić pierwszą literę ciągu wielką, możesz połączyć polecenie `string` z podpoleceniami.

```fish
# Przykładowy ciąg znaków
set sample_string "hello world"

# Zmiana pierwszej litery na wielką
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Wyjście:
```
Hello world
```

W scenariuszach wymagających zmiany wielkości liter wielu słów w ciągu (np. zmiana "hello world" na "Hello World") należałoby iterować przez każde słowo, stosując logikę zmiany wielkości liter do każdego z nich:

```fish
# Przykładowe zdanie
set sentence "hello fish shell programming"

# Zmiana pierwszej litery każdego słowa na wielką
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Łączenie słów z wielką literą
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Wyjście:
```
Hello Fish Shell Programming
```

Warto zauważyć, że Fish Shell nie oferuje bezpośredniej metody do zrobienia pełnej kapitalizacji zdania jednym poleceniem, w taki sposób, jak niektóre języki programowania robią to za pomocą swoich metod pracujących na ciągach znaków. Dlatego połączenie `string split`, `string sub`, `string upper`, a następnie ponowne łączenie reprezentuje idiomatyczne podejście w Fish Shell do osiągnięcia tego.
