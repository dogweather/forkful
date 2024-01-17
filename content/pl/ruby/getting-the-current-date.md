---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Ruby: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Otrzymanie aktualnej daty jest jedną z podstawowych operacji w programowaniu. Programiści używają jej do śledzenia czasu wykonania swojego kodu lub do wyświetlania informacji o dacie dla użytkownika.

## Jak to zrobić:
Poniżej przedstawiono przykłady kodu Ruby, które pokazują różne sposoby pobierania aktualnej daty i wyświetlania jej formatowanej w odpowiedni sposób.

```ruby
Date.today # => #<Date: 2021-08-05 ((2459453j,0s,0n),+0s,2299161j)> # pobiera aktualną datę w formacie obiektu Date
Time.now # => 2021-08-05 14:35:01 +0200 # pobiera aktualną datę i czas w formacie obiektu Time
DateTime.now # => #<DateTime: 2021-08-05T14:35:01+02:00 ((2459453j,14101s,0n),+7200s,2299161j)> # pobiera aktualną datę i czas w formacie obiektu DateTime
```

Możesz również sformatować otrzymane daty i czasy według swoich preferencji, korzystając z metody strftime, która działa dla obiektów Time i DateTime.

```ruby
Time.now.strftime("%d/%m/%Y") # => "05/08/2021" # wyświetla datę w postaci dd/mm/yyyy
DateTime.now.strftime("%I:%M %p") # => "02:35 PM" # wyświetla czas w formacie 12-godzinnym z am/pm
```

## Głębsze nurkowanie:
Pobieranie aktualnej daty niejednokrotnie jest związane z wyświetlaniem jej dla użytkownika lub zapisywaniem jej w bazie danych. Wtedy przydatne może okazać się wykorzystanie gemów takich jak "time" lub "date", które oferują dodatkowe metody do manipulowania datami i czasami.

W alternatywnej implementacji języka Ruby, JRuby, wykorzystywana jest natywna biblioteka Javy - Java Time API - do obsługi dat i czasów.

Aby ułatwić sobie pracę z datami i czasami w Ruby, warto dowiedzieć się więcej o bibliotece standardowej i dostępnych gemach.

## Zobacz także:
- [Dokumentacja Ruby o klasie Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Dokumentacja Ruby o klasie Time](https://ruby-doc.org/stdlib-2.7.1/libdoc/time/rdoc/Time.html)
- [Biblioteka "time" dla Ruby](https://rubygems.org/gems/time)
- [Biblioteka "date" dla Ruby](https://rubygems.org/gems/date)
- [Java Time API dla JRuby](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)