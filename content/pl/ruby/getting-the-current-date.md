---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:35.296837-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"

category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

`Date` w Ruby to klasa pozwalająca na operacje na datach. Programiści często sprawdzają aktualną datę, by rejestrować czas zdarzeń, ustalać terminy czy tworzyć harmonogramy.

## Jak to zrobić:

```Ruby
require 'date'

# Pobranie aktualnej daty
current_date = Date.today
puts current_date
# => 2023-04-01 (przykładowy wynik, będzie różnić się w zależności od dnia uruchomienia)

# Jeśli potrzebujesz większej precyzji, z czasem
require 'time'

current_time = Time.now
puts current_time
# => 2023-04-01 12:34:56 +0200 (przykładowy wynik, będzie różnić się w zależności od momentu uruchomienia i strefy czasowej)
```

## Głębsze spojrzenie:

W przeszłości, przed stdlib `date` i `time`, developerzy Ruby często polegali na zewnętrznych gemach lub własnych implementacjach do zarządzania czasem. Wersja Ruby 1.9 wprowadziła wiele udoskonaleń w klasie `Time`, a `Date` oraz `DateTime` powstały, aby obsłużyć różne potrzeby związane z datami.

Alternatywy dla `Date.today` i `Time.now` mogą obejmować użycie gemu `ActiveSupport` z Rails, który dodaje metody takie jak `Time.zone.now` dla obsługi stref czasowych. Kolejną opcją jest gem `TimeCop` dla testowania, który umożliwia "zamrożenie" czasu na potrzeby testów.

Warto wspomnieć, że `Time.now` zwraca obiekt `Time` z sekundami i frakcjami sekund, a `Date.today` zwraca obiekt `Date` bez informacji o czasie. Ruby używa systemu GMT do przechowywania czasów, co oznacza, że czas jest przechowywany jako liczba sekund od epoki Unix.

## Zobacz również:

- Dokumentacja klasy Time: https://ruby-doc.org/core-2.7.0/Time.html
- Dokumentacja klasy Date: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html
- Ruby API doc o klasie DateTime: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html
- ActiveSupport Core Extensions: https://api.rubyonrails.org/classes/ActiveSupport/CoreExtensions.html
- Dokumentacja TimeCop: https://github.com/travisjeffery/timecop
