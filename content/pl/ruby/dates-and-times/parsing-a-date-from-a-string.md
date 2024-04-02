---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:14.329153-07:00
description: "Przetwarzanie daty z ci\u0105gu znak\xF3w polega na konwersji tekstu,\
  \ kt\xF3ry reprezentuje dat\u0119, na obiekt `Date` lub `DateTime`, kt\xF3ry jest\
  \ zrozumia\u0142y dla Ruby.\u2026"
lastmod: '2024-03-13T22:44:35.941771-06:00'
model: gpt-4-0125-preview
summary: "Przetwarzanie daty z ci\u0105gu znak\xF3w polega na konwersji tekstu, kt\xF3\
  ry reprezentuje dat\u0119, na obiekt `Date` lub `DateTime`, kt\xF3ry jest zrozumia\u0142\
  y dla Ruby.\u2026"
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

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
