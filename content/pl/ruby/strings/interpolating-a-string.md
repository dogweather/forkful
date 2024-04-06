---
date: 2024-01-20 17:51:27.544391-07:00
description: "How to: (Jak to zrobi\u0107:) Interpolacja string\xF3w w Ruby dzia\u0142\
  a tylko w podw\xF3jnych cudzys\u0142owach (\u201E\u201D). Zacz\u0119\u0142a by\u0107\
  \ popularna wraz z Ruby 1.8. Alternatywami\u2026"
lastmod: '2024-04-05T22:50:50.267190-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Interpolacja string\xF3w w Ruby dzia\u0142a tylko\
  \ w podw\xF3jnych cudzys\u0142owach (\u201E\u201D)."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## How to: (Jak to zrobić:)
```Ruby
name = "Łukasz"
greeting = "Cześć, #{name}!"
puts greeting # Wyświetli: Cześć, Łukasz!

temperature = 23
info = "Dzisiaj jest #{temperature} stopni Celsjusza."
puts info # Wyświetli: Dzisiaj jest 23 stopni Celsjusza.

price = 55.89
message = "Całkowity koszt to: #{'%.2f' % price} zł"
puts message # Wyświetli: Całkowity koszt to: 55.89 zł
```

## Deep Dive (W głąb tematu)
Interpolacja stringów w Ruby działa tylko w podwójnych cudzysłowach („”). Zaczęła być popularna wraz z Ruby 1.8. Alternatywami dla interpolacji są konkatenacja (`+`) lub formatowanie stringów za pomocą metody `sprintf` lub globalnej metody `%`. Wydajność interpolacji jest zazwyczaj lepsza niż konkatenacji – Ruby wewnętrznie tworzy nowy string, wstawiając wartości zmiennych. Warto też wiedzieć, że interpolacja automatycznie wywołuje metodę `to_s` na obiekcie, więc zawsze dostajemy string.

## See Also (Zobacz również)
- Ruby Documentation on String Interpolation: [https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings)
