---
title:    "Ruby: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna datum i framtiden eller det förflutna är en viktig färdighet som behövs inom många olika branscher, från programmering till finans. Det låter dig skapa dynamiska applikationer som kan hantera förändringar i tiden och göra uppgifter som schemaläggning enklare.

## Så här gör du
För att beräkna ett datum i framtiden eller det förflutna i Ruby kan du använda metoden `Date#advance` eller `Date#prev`. Till exempel, om vi vill beräkna datumet för fem dagar framåt, kan vi göra det genom att skriva:

```Ruby
require 'date'
today = Date.today
puts today.advance(days: 5)
# Output: 2021-11-25
```

På samma sätt kan vi använda `Date#prev` för att beräkna ett datum i det förflutna genom att ange ett negativt värde för antal dagar. Till exempel, om vi vill veta datumet för en vecka sedan, kan vi skriva:

```Ruby
require 'date'
today = Date.today
puts today.prev(day: 7)
# Output: 2021-11-18
```

Det är viktigt att notera att dessa metoder endast returnerar ett datumobjekt och inte gör några permanenta förändringar i själva datumet.

## Djupdykning
Ruby har också en annan inbyggd metod som heter `Date#strftime` som låter dig formatera datumet som du vill ha det. Du kan använda det tillsammans med `Date#advance` och `Date#prev` för att ange en speciell formatsträng som bestämmer hur datumet ska visas.

Till exempel, om du vill ha datumet i formatet DD/MM/YYYY, kan du använda:

```Ruby
require 'date'
today = Date.today
puts today.advance(days: 5).strftime("%d/%m/%Y")
# Output: 25/11/2021
```

För att läsa mer om `Date#advance`, `Date#prev` och `Date#strftime`, kan du titta på dokumentationen för Ruby.

## Se även
- [Date and time in Ruby](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-ruby)
- [Ruby DateTime class](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Date and time formatting in Ruby](https://www.rubyguides.com/2015/02/ruby-time-format/)