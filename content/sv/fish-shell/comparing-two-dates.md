---
title:                "Fish Shell: Jämförande av två datum"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en vanlig uppgift inom programmering, och kan vara användbart för att sortera och filtrera data, eller för att kontrollera om vissa åtgärder ska utföras baserat på datum. I denna bloggpost kommer vi att utforska hur man jämför två datum i Fish Shell.

## Så här gör du

För att jämföra två datum i Fish Shell, behöver vi först konvertera dem till formatet som Fish Shell förstår. Detta kan göras med hjälp av `date` kommandot och flaggorna `-f` för att ange formatet och `-j` för att returnera datumet i Unix-timestamp format. Låt oss titta på ett exempel:

```
set start_date (date -f %Y-%m-%d -j 2021-01-01)
set end_date (date -f %Y-%m-%d -j 2021-12-31)
```

I detta exempel har vi definierat två variabler som innehåller start- och slutdatum i Unix-timestamp format. Nu kan vi jämföra dem med hjälp av Fish Shell's `test` kommando och flaggan `-gt` för större än. Om startdatumet är större än slutdatumet, kommer testet att returnera `true`:

```
test $start_date -gt $end_date; echo $status
```

Outputen av detta kommer att vara `1`, vilket betyder att testet returnerade `false`.

## Djupdykning

När du jämför två datum i Fish Shell, är det viktigt att se till att de är i samma format. Om de har olika format, kommer jämförelsen att ge felaktiga resultat. Dessutom bör man vara medveten om att datumen kan inkludera tidsinformation, vilket kan påverka jämförelsen. Det är alltid bäst att konvertera datumen till Unix-timestamp format för att undvika felaktigheter.

Det finns också andra sätt att jämföra datum i Fish Shell, såsom att använda `sort` kommandot med flaggan `-n` för numeriskt sorterade och sedan välja det första eller sista värdet för att jämföra med det önskade datumet.

## Se även

- [Fish Shell Date Kommando](https://fishshell.com/docs/current/commands.html#date)
- [Unix Timestamp](https://www.unixtimestamp.com/index.php)