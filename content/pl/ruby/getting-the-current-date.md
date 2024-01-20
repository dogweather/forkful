---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty to proces odczytywania aktualnej daty z systemu operacyjnego. Programiści korzystają z tego, aby śledzić daty i czasy wydarzeń, logi systemowe, a także generować dynamiczne treści.

## Jak zrobić:

 Zobaczmy, jak w języku Ruby pobrać aktualną datę:

```Ruby
# Dzisiejsza data
require 'date'
puts Date.today
```

Tak więc, za pomocą wbudowanej biblioteki `date` i metody `today`, możemy wywołać bieżącą datę. Po uruchomieniu powyższego kodu, otrzymasz coś w stylu:
  
```Ruby
2025-08-30
```

## Wgłębiając się

1. **Historia**: Z początku, odczytywanie daty było dość kłopotliwe i złożone. Wraz z rozwojem technologii i języków programowania, takich jak Ruby, stało się to łatwiejsze i bardziej efektywne. 

2. **Alternatywy**: Istnieją alternatywne sposoby na pobieranie bieżącej daty w rubym, np. za pomocą metody `Time.now`.
 
   ```Ruby
    # Dzisiejsza data
    puts Time.now
    ```

3. **Szczegóły implementacji**: `Date.today()` działa, lokalizując Twój czas systemowy i strefę czasową, a następnie konwertuje je na obiekt `Date`. Metoda ta jest tylko do odczytu i nie wpłynie na czas systemowy.

## Zobacz także

1. Oficjalna dokumentacja Ruby dla klasy [Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)

2. Dokumentacja Ruby dla klasy [Time](https://ruby-doc.org/core-2.5.0/Time.html)

3. Doskonałe [szczegółowe omówienie](https://www.rubyguides.com/2015/12/ruby-time/) obsługi daty i godziny w Ruby na RubyGuides.