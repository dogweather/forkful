---
title:                "Ruby: Omvandling av datum till sträng"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med Ruby-programmering, är det ofta nödvändigt att konvertera datum till en sträng eller textformat. Detta kan vara användbart när man till exempel behöver skriva ut datum i en rapport eller spara datuminformation i en databas. I denna artikel kommer vi att utforska hur man konverterar en datumvariabel till en sträng i Ruby.

## Hur man gör det

För att konvertera en datumvariabel till en sträng i Ruby, kan du använda metoderna `strftime` och `to_s`. `strftime` står för "format date and time" och används för att anpassa datum till ett specifikt format. `to_s` betyder "to string" och konverterar en variabel till en sträng.

Låt oss säga att vi har en variabel som innehåller dagens datum:

```Ruby 
today = Date.today 
```

För att konvertera detta datum till en sträng i formatet `ÅR-MÅNAD-DAG` skulle koden se ut såhär:

```Ruby
today.strftime("%Y-%m-%d")
```

Detta kommer att ge oss en sträng som ser ut såhär: `2021-03-15`.

Du kan också kombinera flera delar av datumet för att få en mer detaljerad sträng. Till exempel, om du vill ha dagen på engelska och året i fyrsiffrigt format, kan du använda detta kodsnutt:

```Ruby
today.strftime("%A, %Y")
```

Detta kommer att ge oss en sträng som ser ut såhär: `Monday, 2021`.

## DJupdykning

I de tidigare kodsnutten har vi använt oss av metoden `strftime` tillsammans med ett argument som kallas för "formatsträngen". Denna sträng består av olika symboler som representerar olika delar av datumet. Till exempel, `%Y` står för året och `%m` står för månaden. För en mer detaljerad lista över dessa symboler kan du titta på Ruby dokumentationen.

En annan viktig sak att tänka på är att `strftime` bara kan användas på datumvariabler och inte på tidsvariabler. Om du vill konvertera en tidsvariabel till en sträng kan du använda metoden `to_s`.

## Se även

Här är några länkar som kan vara användbara när du arbetar med datum och strängar i Ruby:

- [Ruby dokumentation om `strftime`](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-strftime)
- [En guide till olika formatsträngar för datum i Ruby](https://alvinalexander.com/technology/how-to-use-date-time-format-string-ruby-rails/)
- [Mer information om `to_s` metoden i Ruby](https://ruby-doc.org/core-2.7.2/Object.html#method-i-to_s)