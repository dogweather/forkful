---
title:                "Ruby: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV-filer är ett vanligt format för att hantera data i olika programmeringsprojekt. Genom att lära dig att arbeta med CSV-filer kan du enkelt importera och exportera data från dina program. Detta är praktiskt om du behöver dela data med andra eller om du behöver manipulera stora mängder data.

## Hur du gör det

Att arbeta med CSV-filer i Ruby är enkelt. Först måste du ladda ner och installera csv-paketet genom att köra följande kommando i terminalen:

```Ruby
gem install csv
```

När paketet är installerat kan du öppna en CSV-fil och läsa eller skriva data till den. Här är ett exempel på hur man läser en CSV-fil och skriver ut varje raderar till konsolen:

```Ruby
require 'csv'
CSV.foreach("min_fil.csv") do |rad|
  puts rad
end
```

För att skriva till en CSV-fil, kan du använda metoden `CSV.open` och ange lämpliga filnamn och lägg till de data du vill skriva till filen. Här är ett exempel på hur man lägger till nya rader till en CSV-fil:

```Ruby
require 'csv'
CSV.open("ny_file.csv", "w") do |csv|
  csv << ["namn", "ålder", "land"]
  csv << ["Anna", 25, "Sverige"]
  csv << ["Peter", 30, "Danmark"]
end
```

## Djupdykning

Förutom att läsa och skriva från CSV-filer, finns det många andra funktioner som hjälper dig att manipulera data i CSV-format. Du kan till exempel välja specifika kolumner eller rader, filtrera eller sortera data och ändra formatet på celler. Du kan också använda CSV-modulen för att skapa egna CSV-tillämpningar eller integrera den med andra bibliotek och ramverk.

Det finns många resurser online som kan hjälpa dig att lära dig mer om hur man arbetar med CSV-filer i Ruby, inklusive dokumentationen för CSV-paketet och olika guider och tutorials.

## Se också

- [Dokumentation för CSV-paketet](https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html)
- [En guide till att arbeta med CSV-filer i Ruby](https://www.rubyguides.com/2018/10/working-with-csv-files-in-ruby/) 
- [Introduktion till Ruby-programmering](https://www.codecademy.com/learn/learn-ruby)