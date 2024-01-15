---
title:                "Jämföring av två datum"
html_title:           "Ruby: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför 

Det är vanligt att man behöver jämföra två datum i Ruby-programmering. Det kan vara för att kontrollera om ett datum har passerat, eller för att sortera en lista med datum. 

## Hur man gör 

För att jämföra två datum i Ruby kan man använda sig av metoden `Date#<=>`. Den kommer att returnera en negativ, noll eller positiv siffra beroende på om det första datumet är tidigare, samma dag eller senare än det andra datumet. 

```Ruby
today = Date.today
future_date = Date.parse('2021-12-25')
past_date = Date.new(2021, 1, 1)

# Jämför idag med ett datum i framtiden
today <=> future_date # returnerar -1

# Jämför två framtida datum
future_date <=> Date.parse('2022-01-01') # returnerar -1

# Jämför två förflutna datum
past_date <=> Date.new(2020, 12, 25) # returnerar 1

# Jämför två lika datum
today <=> Date.today # returnerar 0
```

Man kan även använda sig av klassen `DateTime` för att jämföra datum och tider. Syntaxen för jämförelse är densamma som för `Date`.

```Ruby
current_time = DateTime.now
future_time = DateTime.new(2022, 1, 1, 12, 0, 0)

# Jämför nuvarande tid med en framtida tid
current_time <=> future_time # returnerar -1
```

## Djupdykning 

Metoden `Date#<=>` och `DateTime#<=>` använder sig av en metod som heter `__cmp__` som jämför två datum och returnerar en negativ, noll eller positiv siffra beroende på resultatet. Man kan också använda sig av `Date#eql?` och `DateTime#eql?` för att kontrollera om två datum är exakt lika eller inte. 

Det är också värt att notera att `Date` och `DateTime` är två olika klasser i Ruby, och har därför olika metoder för att hantera datum och tider. Det är viktigt att använda rätt klass beroende på vad man behöver jämföra. 

## Se även 

- [Ruby Date och Time klasser](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Kolla om ett datum har passerat i Ruby](https://medium.com/@cecilgj/how-to-check-if-date-has-passed-in-ruby-949f087b6a7b)