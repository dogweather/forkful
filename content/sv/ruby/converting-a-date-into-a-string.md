---
title:    "Ruby: Konvertera ett datum till en sträng"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift när man programmerar med Ruby. Det kan vara användbart för att visa datum på ett mer läsbart sätt eller för att hantera information från olika källor.

## Hur man gör det

För att konvertera ett datum till en sträng i Ruby, kan man använda sig av metoden `strftime`. Det står för "stringfy time" och låter dig formatera en tid eller ett datum som en sträng. Här är ett exempel på hur man kan använda det:

```Ruby
date = Time.now
date_string = date.strftime("%d %B %Y")
puts date_string
```

Detta kommer att ge oss en sträng som ser ut så här: "07 juli 2021". Den första delen av metoden `strftime` är formatet du vill ha din sträng i, i vårt fall vill vi ha dag, månad och år. Det andra argumentet är det datum du vill konvertera till en sträng, i vårt fall använder vi `Time.now` som ger oss dagens datum.

## Djupdykning

Som vi såg i det föregående exemplet, kan vi använda `strftime` för att formatera datumet på ett visst sätt. Men det finns också flera fördefinierade format som vi kan använda. Till exempel kan vi använda `%x` för att få en mer kortfattad version av datumet som "07/07/2021" eller `%A` för att få veckodagen som "onsdag".

Det finns också andra metoder för att konvertera datum till strängar, som `to_s` och `to_formatted_s`. Det är viktigt att testa och jämföra olika metoder för att hitta den som passar bäst för ditt specifika projekt.

## Se också

* [Time Class (Ruby 3.0.2)](https://ruby-doc.org/core-3.0.2/Time.html)
* [Ruby strftime documentation](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)
* [Converting Between DateTime, Date, Time and ActiveSupport::TimeWithZone](https://blog.appsignal.com/2021/04/07/activerecord-datetime-date-time-activerecord-time-with-zone.html)