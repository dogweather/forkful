---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

"## Co i dlaczego?"
Przekształcanie łańcuchów znaków na małe litery to proces zmiany wszystkich dużych liter w łańcuchu na ich odpowiedniki w niższej formie. Dlaczego programiści to robią? Często, aby ułatwić porównanie ciągów lub normalizację danych.

"## Jak to zrobić:"
Aby przekształcić łańcuch znaków na małe litery w Ruby, użyjemy metody downcase. To proste!

```Ruby
tekst = "TU JEST JAKIŚ TEKST"
puts tekst.downcase
```

Wyjście:

```Ruby
tu jest jakiś tekst
```

"## Głębsze zrozumienie"
**Kontekst historyczny**: Przekształcanie ciągów na małe litery (i większe) jest jednym z podstawowych procesów przetwarzania tekstu, stosowanych od początku ery komputerowej.

**Alternatywy**: Ruby oferuje także metody takie jak downcase!, która zmieni na małe litery oryginalny ciąg znaków, a nie tworzy nowego. 

**Szczegóły implementacji**: Metoda downcase działa na obiektach String, które są niezmienne w Ruby. Dlatego metoda ta zwraca nowy obiekt String, pozostawiając oryginalny łańcuch niezmienionym.

"## Zobacz także"
Wybierz jedno z poniższych źródeł, aby dowiedzieć się więcej o pracy z łańcuchami znaków w Ruby:
1. Dokumentacja języka Ruby: [String — Ruby 3.0.0](https://ruby-doc.org/core-3.0.0/String.html)
2. "Programming Ruby": [Operacje na łańcuchach](http://ruby-doc.com/docs/ProgrammingRuby/)