---
title:                "Ruby: Przetwarzanie daty na ciąg znaków"
simple_title:         "Przetwarzanie daty na ciąg znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Dlaczego

Jeśli programujesz w Ruby, prawdopodobnie już zetknąłeś się z koniecznością konwertowania daty na ciąg znaków. Jest to bardzo częste zadanie, które może być wykonywane z różnych powodów, na przykład do wyświetlenia daty w czytelnej formie dla użytkownika lub do zapisania jej w bazie danych.

##Jak to zrobić

Istnieje kilka sposobów na dokonanie konwersji daty na ciąg znaków w Ruby. Jedną z najprostszych metod jest użycie metody `strftime`, która jest dostępna dla obiektów typu `Date` i `Time`. Poniżej przedstawiam przykładowy kod wykorzystujący tę metodę:

```Ruby
require 'date'

current_date = Date.today
string_date = current_date.strftime("%d/%m/%Y")

puts string_date # Wyświetli "29/03/2021" 
```

Jak widać, używając parametrów `%d`, `%m` i `%Y`, możemy określić format, w jakim chcemy wyświetlić datę. Istnieje wiele innych opcji formatowania, możemy na przykład dodać nazwę dnia tygodnia lub skrócony format miesiąca. Wszystkie dostępne parametry można znaleźć w dokumentacji języka Ruby.

##Głębszy wgląd

Podczas konwertowania daty na ciąg znaków ważne jest, aby pamiętać o strefie czasowej. W Ruby domyślnie używana jest strefa czasowa systemu operacyjnego, jednak możemy ją zmienić za pomocą metody `Time.zone=`. Warto również zwrócić uwagę na format daty, ponieważ w różnych krajach stosowane są różne konwencje. 

Warto również wspomnieć, że w nowszych wersjach Ruby istnieje również metoda `to_s` dla obiektów typu `Date` i `Time`, która również pozwala na konwersję daty na ciąg znaków.

##Zobacz również

- [Dokumentacja Ruby o metodzie `strftime`](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Dokumentacja Ruby o strefach czasowych](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Poradnik o konwersji daty na ciąg znaków w Ruby](https://www.rubyguides.com/2015/09/ruby-date-format/)