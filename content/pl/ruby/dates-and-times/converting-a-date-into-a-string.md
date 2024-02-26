---
date: 2024-01-20 17:37:31.281257-07:00
description: "Zamiana daty na \u0142a\u0144cuch znak\xF3w (string) to proces przekszta\u0142\
  cania obiektu daty w czytelny tekst. Programi\u015Bci robi\u0105 to, \u017Ceby u\u0142\
  atwi\u0107 wy\u015Bwietlanie dat\u2026"
lastmod: '2024-02-25T18:49:34.320149-07:00'
model: gpt-4-1106-preview
summary: "Zamiana daty na \u0142a\u0144cuch znak\xF3w (string) to proces przekszta\u0142\
  cania obiektu daty w czytelny tekst. Programi\u015Bci robi\u0105 to, \u017Ceby u\u0142\
  atwi\u0107 wy\u015Bwietlanie dat\u2026"
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zamiana daty na łańcuch znaków (string) to proces przekształcania obiektu daty w czytelny tekst. Programiści robią to, żeby ułatwić wyświetlanie dat użytkownikom i przechowywanie dat w bazach danych w ustandaryzowanym formacie.

## How to: (Jak to zrobić:)
```Ruby
require 'date'

# Tworzenie nowej daty
date = Date.new(2021, 12, 25)

# Zamiana na łańcuch znaków w domyślnym formacie
date_string = date.to_s
puts date_string # => "2021-12-25"

# Formatowanie daty zgodnie z własnymi preferencjami
formatted_date_string = date.strftime('%d-%m-%Y')
puts formatted_date_string # => "25-12-2021"

# Inny przykład formatowania - nazwa dnia tygodnia, dzień miesiąca i rok
another_format = date.strftime('%A, %d of %B, %Y')
puts another_format # => "Saturday, 25 of December, 2021"
```

## Deep Dive (Dogłębna analiza):
W Ruby, klasa `Date` i moduł `Time` zawierają metody do manipulacji datami. Kiedy Ruby powstawało, istniała potrzeba łatwego konwersji pomiędzy datami a tekstowymi reprezentacjami. Stąd `to_s` i `strftime`, która jest bardziej elastyczna.

Alternatywą dla `strftime` jest użycie różnych gemów, takich jak `time_ago_in_words` z Rails, które dostarczają bardziej zrozumiałe formaty dla użytkowników.

W implementacji, `strftime` opiera się na formaterze C, który jest bardzo wydajny, ale wymaga znajomości specyficznych dyrektyw formatujących.

## See Also (Zobacz też):
- Ruby Docs dla klasy Date: https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html
- Ruby Docs dla klasy Time: https://ruby-doc.org/core-3.0.0/Time.html
- Gem `Time_ago_in_words` dla Rails: https://apidock.com/rails/ActionView/Helpers/DateHelper/time_ago_in_words
