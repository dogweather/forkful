---
title:                "Interpolacja łańcuchów znaków"
aliases:
- /pl/ruby/interpolating-a-string/
date:                  2024-01-20T17:51:27.544391-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
String interpolation w Ruby pozwala wrzucać zmienne lub wyrażenia do środka tekstu. Używamy tego, by łatwo tworzyć dynamiczne stringi.

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
