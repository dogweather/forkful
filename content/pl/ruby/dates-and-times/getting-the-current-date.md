---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.412192-07:00
description: "Jak to zrobi\u0107: Standardowa biblioteka Ruby zawiera klasy `Date`\
  \ i `Time` do obs\u0142ugi dat i czasu. Oto jak uzyska\u0107 bie\u017C\u0105c\u0105\
  \ dat\u0119."
lastmod: '2024-03-13T22:44:35.942850-06:00'
model: gpt-4-0125-preview
summary: "Standardowa biblioteka Ruby zawiera klasy `Date` i `Time` do obs\u0142ugi\
  \ dat i czasu."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:
Standardowa biblioteka Ruby zawiera klasy `Date` i `Time` do obsługi dat i czasu. Oto jak uzyskać bieżącą datę:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Przykładowe wyjście: 
```
2023-04-12
```

Aby uwzględnić czas wraz z datą, bardziej odpowiednia jest klasa `Time` w Ruby:

```ruby
current_time = Time.now
puts current_time
```

Przykładowe wyjście: 
```
2023-04-12 14:33:07 +0200
```

Jeśli potrzebujesz większej funkcjonalności, na przykład zarządzania strefami czasowymi, możesz chcieć użyć zewnętrznego gema, jak na przykład `ActiveSupport` (część Rails, ale można go używać samodzielnie).

Najpierw dodaj `activesupport` do swojego pliku Gemfile i uruchom `bundle install`:

```ruby
gem 'activesupport'
```

Następnie użyj go do obsługi stref czasowych:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Ustaw żądaną strefę czasową
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Przykładowe wyjście:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
