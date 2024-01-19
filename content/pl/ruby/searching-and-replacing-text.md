---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana i wyszukiwanie tekstu to podstawowe operacje, które przeprowadza większość programistów. Ułatwiają one manipulowanie danymi i modyfikowanie informacji przetwarzanych przez program.

## Jak to zrobić:

Ruby oferuje kilka sposobów na wyszukiwanie i zamianę tekstu. Najpopularniejsze to metody `gsub` i `gsub!`. Oto przykład:

```Ruby
str = "Cześć, Świecie!"
str.gsub!('Świecie', 'Ruby')
puts str
```

Wyjście:

```
Cześć, Ruby!
```
Jak widać, `gsub!` zastępuje każde wystąpienie słowa 'Świecie' słowem 'Ruby' w oryginalnym łańcuchu.

## Głębsze spojrzenie:

1. Kontekst historyczny: Ruby, stworzony w 1995 roku przez Yukihiro Matsumoto, od początku oferował potężne metody manipulowania ciągami znaków, takie jak `gsub` i `gsub!`.

2. Alternatywy: Można również skorzystać z metody `sub` lub `sub!`, które zastąpią tylko pierwsze wystąpienie dopasowanego wzorca.

3. Szczegóły implementacji: Metoda `gsub` (znaczy "global substitute") używa wyrażeń regularnych do identyfikacji wzorców w tekście, które mają zostać zastąpione. Jeśli użyjesz wariantu z wykrzyknikiem (`gsub!`), modyfikuje to bezpośrednio oryginalny ciąg zamiast zwracać nowy.

## Zobacz także:

1. [Dokumentacja Ruby: String#gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)

2. [Dokumentacja Ruby: Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html)

3. [Wyszukiwanie i zamiana tekstu w Rubim](https://www.rubyguides.com/2019/07/ruby-gsub-method/)