---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Ruby: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

### Co i dlaczego?

Konwertowanie daty na ciąg znaków to proces, w którym data zostaje przekształcona z formatu, który komputery mogą odczytać, na czytelny dla ludzi ciąg znaków. Programiści często wykonują tę operację, ponieważ wymaga tego np. wyświetlanie dat w aplikacjach lub zapisywanie ich w bazach danych.

### Jak to zrobić:

```ruby
require 'date'

date = Date.today
puts date.to_s

# Sample Output: "2020-10-08"
```

Możemy użyć wbudowanej biblioteki 'date' i metody `to_s` aby przekonwertować datę na ciąg znaków.

### Głębszy zanurzenie:

Konwersja daty na ciąg znaków jest istotnym elementem programowania od samego początku jego istnienia. Początkowo, w językach takich jak Fortran czy Cobol, operacje na datach były bardzo skomplikowane i czasochłonne. Jednak dzięki rozwojowi technologii i wprowadzeniu nowych bibliotek programistycznych, konwersja dat stała się znacznie łatwiejsza.

Alternatywnym sposobem konwertowania daty jest użycie zewnętrznych bibliotek, takich jak Moment.js czy Datejs. Dzięki nim można łatwo przekonwertować datę na różne formaty i również manipulować nią w inny sposób.

W Ruby, daty są przechowywane w postaci liczby, która reprezentuje liczbę dni, która minęła od 1 stycznia 4713 roku p.n.e. W metodzie `to_s`, liczba ta jest przekonwertowana na czytelny dla ludzi format.

### Zobacz również:

- Dokumentacja Ruby o konwersji dat: https://ruby-doc.org/core-2.7.1/Date.html#method-i-to_s
- Biblioteka Moment.js do manipulacji i formatowania dat w języku JavaScript: https://momentjs.com/
- Dokumentacja Ruby o klasie Date: https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html