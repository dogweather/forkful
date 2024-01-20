---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Rzutowanie daty na łańcuch znaczy konwertowanie obiektu typu data na ciąg znaków. Programiści robią to, by ułatwić wysyłanie i odczytywanie dat w interfejsach użytkownika, plikach i bazach danych.

## Jak to zrobić:

```Ruby
require 'date'

data = Date.new(2022, 10, 23)
data_na_lancuch = data.to_s

puts data_na_lancuch
# Wypisze: 2022-10-23
```
W powyższym kodzie, korzystamy z metody `to_s` wbudowanej w klasę Date, żeby zamienić datę na łańcuch.

```Ruby
require 'date'

data = DateTime.new(2022, 10, 23, 14, 30)
data_na_lancuch = data.strftime("%d/%m/%Y %H:%M")

puts data_na_lancuch
# Wypisze: 23/10/2022 14:30
```
Tutaj używamy metody `strftime`, której możemy podać format wyjścia, żeby uzyskać łańcuch w pożądanym formacie.

## Dogłębne Zanurzenie:

W przeszłości, w rubym używano wyłącznie `to_s` do rzutowania daty na łańcuch. Prowadziło to jednak do problemów z nieoczekiwanym formatowaniem i brakiem uniwersalności. Wprowadzenie `strftime` umożliwiło bardziej precyzyjne formatowanie łańcuchów. 

Alternatywą dla `strftime` jest `iso8601`, który zwraca łańcuch w formacie ISO 8601, standardzie wykorzystywanym w systemach wymagających dużej zgodności. 

Szczegóły implementacyjne metody `strftime` są dosyć skomplikowane, ale w skrócie, każdy znak 'dyrektywy' (np. %d, %m, %Y) jest zamieniany na odpowiadającą mu wartość z daty, a następnie wszystko jest składane razem w ostateczny łańcuch.

## Zobacz Również:

- Dokumentacja Ruby o Date: https://ruby-doc.org/stdlib-3.0.3/libdoc/date/rdoc/Date.html
- Przewodnik Ruby o formatowaniu dat: https://www.rubyguides.com/2015/12/ruby-time/
- Dyskusja na StackOverflow o różnicy między `to_s`, `strftime` i `iso8601`: https://stackoverflow.com/questions/1304505/difference-between-date-to-s-and-date-strftime