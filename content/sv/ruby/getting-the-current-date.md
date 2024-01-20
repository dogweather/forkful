---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:29.106107-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Hämta dagens datum innebär att du får det nuvarande datumet från ditt system. Programmerare gör detta för att hantera tidsberoende funktioner eller för att logga och spåra händelser.

## Så här gör du:
Att få dagens datum i Ruby är smidigt. Använd `Date.today` för enkelt datum eller `DateTime.now` för datum och tid.

```Ruby
require 'date'

# Enbart datum
puts Date.today
# => 2023-04-12

# Datum och tid
puts DateTime.now
# => 2023-04-12T15:24:35+02:00
```

## Djupdykning
I tidiga Ruby-versioner använde man ofta `Time.now` för både datum och tid, men `DateTime` och `Date` introducerades för att hantera en större variation av datum- och tidsoperationer. Alternativ såsom Time zones och format kan hanteras med tilläggsbibliotek som 'active_support'. Att förstå skillnaden mellan `Date`, `Time`, och `DateTime` är nyckeln till att välja rätt verktyg för uppgiften.

## Se även
- Ruby-dokumentation för `Date`: [https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- Ruby-dokumentation för `DateTime`: [https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)