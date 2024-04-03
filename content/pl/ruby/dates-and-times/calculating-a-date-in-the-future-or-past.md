---
date: 2024-01-20 17:31:49.011035-07:00
description: "Jak to zrobi\u0107: Ruby \u015Bmiga z datami jak \u0142y\u017Cwiarz\
  \ na lodzie. U\u017Cyj `Date` i `Time`, plus troch\u0119 matematyki. Zobacz na przyk\u0142\
  ady."
lastmod: '2024-03-13T22:44:35.945725-06:00'
model: gpt-4-1106-preview
summary: "Ruby \u015Bmiga z datami jak \u0142y\u017Cwiarz na lodzie."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

## Jak to zrobić:
Ruby śmiga z datami jak łyżwiarz na lodzie. Użyj `Date` i `Time`, plus trochę matematyki. Zobacz na przykłady:

```Ruby
require 'date'

# Dodaj dni do aktualnej daty
future_date = Date.today + 10
puts future_date.to_s  # np. "2023-04-15"

# Odejmij dni od aktualnej daty
past_date = Date.today - 10
puts past_date.to_s  # np. "2023-03-26"

# Dodaj sekundy do aktualnego czasu
future_time = Time.now + (60 * 60 * 24 * 10)
puts future_time.strftime("%Y-%m-%d %H:%M:%S")  # np. "2023-04-15 14:28:36"

# Odejmij sekundy od aktualnego czasu
past_time = Time.now - (60 * 60 * 24 * 10)
puts past_time.strftime("%Y-%m-%d %H:%M:%S")  # np. "2023-03-26 14:28:36"
```

## Deep Dive
Daty są tricky, ale nie w Ruby. Używa się `Date` dla dat (bez czasu) i `Time` dla dokładnej godziny. Ruby, od wersji 1.9, ma wbudowaną obsługę stref czasowych i precyzję co do milisekund.

Wcześniej był standard `Time` z 1970 roku, "epocha" nazwana Unix Time. Nie obsługiwał stref ani dat przed 1970, ale `Time` w Ruby już tak.

Inne języki mają biblioteki typu DateTime w .NET lub moment.js w JavaScript, ale Ruby trzyma to prosto. Jest jeszcze `ActiveSupport::TimeWithZone` w Rails dla stref czasowych.

Kombinowaniem z datami jest jak malowanie ogrodu - trzeba wiedzieć, co robić. Na przykład, przejście na czas letni/zimowy. Ruby sobie z tym radzi, ale musisz być uważny.

## Zobacz też
- Ruby's Date class: https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html
- Ruby's Time class: https://ruby-doc.org/core-3.0.0/Time.html
- ActiveSupport::TimeWithZone documentation: https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html
- Time zones in Ruby on Rails: https://guides.rubyonrails.org/time_zone_awareness.html
