---
title:    "Ruby: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller förflutet är en användbar färdighet inom programmering. Det kan hjälpa till att skapa dynamiska applikationer och underlätta hanteringen av tidsrelaterade uppgifter.

## Hur man gör det
För att kunna beräkna ett datum i framtiden eller förflutet använder man sig av Ruby's inbyggda `Date` klass. Här är ett exempel på hur man beräknar ett datum som är två veckor från nu:

```Ruby
today = Date.today
future_date = today + 14
puts future_date
```

Detta kodblock kommer att producera följande output:

```
2021-08-22
```

För att beräkna ett datum i förflutet använder man subtraction istället. Här är ett exempel på hur man beräknar ett datum som var två månader sedan:

```Ruby
today = Date.today
past_date = today - 60
puts past_date
```

Detta kommer att ge följande output:

```
2021-04-25
```

## Djupdykning
När vi använder oss av `Date` klassen i Ruby, så kan vi också gå in på ett specifikt datum och ändra eller hämta specifika delar av det. Till exempel kan vi ändra månaden på ett datum eller hämta vilken dag det är på ett visst datum. Detta görs genom att använda sig av metoder som `month` och `day`. Här är ett exempel på hur man kan ändra månaden på ett datum:

```Ruby
date = Date.today
puts date.month #output: 8
date = date.change(month: 12)
puts date #output: 2021-12-22
puts date.month #output: 12
```

Detta är bara en av många möjliga användningar av `Date` klassen när man ska beräkna datum i framtiden eller förflutet. Det finns många fler metoder och funktioner som kan utforskas för att skapa mer dynamiska och exakta datumberäkningar.

## Se även
- [Ruby's Date Class Documentation](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
- [Calculating Time Differences in Ruby](https://www.rubyguides.com/2019/10/ruby-time-difference/)
- [Working with Dates and Times in Ruby](https://www.twilio.com/blog/working-with-dates-and-times-in-ruby)