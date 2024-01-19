---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Ruby: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przewidywanie daty w przyszłości lub przeszłości to mechanizm, który umożliwia programistom ustalanie dat poza bieżącą datą systemu. Programiści robią to, aby zarządzać zadaniami i wydarzeniami, które są czasowo zależne.

## Jak to zrobić:

W Ruby możemy łatwo obliczyć datę w przeszłości lub przyszłości, stosując metody `days`, `months` i `years` dla obiektów `Date` i `Time`.

```ruby
require 'date'

dzisiaj = Date.today
przyszlosc = dzisiaj + 30 #30 dni do przodu
przeszlosc = dzisiaj - 20 #20 dni do tyłu

puts dzisiaj
puts przyszlosc
puts przeszlosc
```

Kiedy uruchomisz ten skrypt, zobaczysz coś podobnego do tego:

```ruby
#2022-01-01
#2022-01-31
#2021-12-12
```
## Pogłębione informacje:

1. **Kontekst historyczny**: Ruby od początku zawierał mechanizmy do manipulowania datami i czasem, zwracając uwagę na czytelność i łatwość użycia kodu. 

2. **Alternatywy**: Inne języki, takie jak Python i Java, mają również własne metody do obliczania dat w przeszłości i przyszłości. W tych językach metoda może wyglądać inaczej, ale idea jest ta sama.

3. **Szczegóły implementacji**: Ruby używa biblioteki `date`, która udostępnia zestaw klas (Date, DateTime, etc.) służących do manipulowania datami. Wywołując metodę `+` lub `-` na obiekcie Date, tworzysz nowy obiekt Date, nie modyfikując oryginalnej daty.

## Zobacz także:

1. Dokumentacja Ruby - Klasa Date: [https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
2. Dokumentacja Ruby - Metody czasu i daty: [https://ruby-doc.org/core-2.5.1/Time.html](https://ruby-doc.org/core-2.5.1/Time.html)
3. Tutorial Ruby - Praca z datami i czasem: [https://www.tutorialspoint.com/ruby/ruby_date_time.htm](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)