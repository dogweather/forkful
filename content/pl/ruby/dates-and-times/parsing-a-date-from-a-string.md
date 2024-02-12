---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases: - /pl/ruby/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:14.329153-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie daty z ciągu znaków polega na konwersji tekstu, który reprezentuje datę, na obiekt `Date` lub `DateTime`, który jest zrozumiały dla Ruby. Programiści robią to, aby wykonywać operacje takie jak porównania, obliczenia czy formatowanie dat, co jest częstym zadaniem w aplikacjach zajmujących się planowaniem, analizą czy przetwarzaniem danych.

## Jak to zrobić:
W Ruby standardowa biblioteka zapewnia bezpośrednie sposoby na przetwarzanie dat z ciągów znaków przy użyciu klas `Date` i `DateTime`. Oto jak to zrobić za pomocą wbudowanych metod Ruby:

```ruby
require 'date'

# Przetwarzanie daty z ciągu znaków
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime dla bardziej szczegółowego przedstawienia czasu
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Dla większej kontroli lub aby obsłużyć formaty, których metoda `parse` może nie rozumieć bezpośrednio, można użyć `strptime` (string parse time), określając format wyraźnie:

```ruby
# Używanie strptime dla niestandardowych formatów
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Korzystanie z bibliotek stron trzecich:

Chociaż wbudowane możliwości Ruby są potężne, czasami możesz preferować biblioteki stron trzecich dla dodatkowych funkcji lub prostszej składni. Popularnym wyborem jest gem `Chronic` do parsowania języka naturalnego:

1. Najpierw dodaj Chronic do pliku Gemfile i uruchom `bundle install`:
```ruby
gem 'chronic'
```

2. Następnie użyj go w taki sposób:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# Wynik będzie zależał od aktualnej daty; zakłada przetwarzanie na 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` jest bardzo użyteczny dla danych wejściowych użytkownika, ponieważ może rozumieć szeroki zakres formatów dat w języku naturalnym, co czyni go potężnym narzędziem dla aplikacji wymagających elastycznego wprowadzania dat.
