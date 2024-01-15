---
title:                "Arbeta med csv"
html_title:           "Ruby: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Om du är nybörjare i programmering eller bara vill lära dig ett nytt programmeringsspråk, kan det vara bra att lära sig hur man arbetar med CSV-filer. CSV, eller Comma Separated Values, är ett vanligt format för att lagra och överföra data i tabellform. Det kan användas för allt från enkel datahantering till avancerade databaser.

## Så här gör du

För att kunna arbeta med CSV-filer i Ruby behöver du först importera "csv" biblioteket i din kod:

```Ruby
require 'csv'
```

För att öppna och läsa en CSV-fil, kan du använda följande kod:

```Ruby
CSV.foreach("filnamn.csv") do |rad|
  p rad
end
```

Detta kommer att skriva ut varje rad i CSV-filen som en array. Om du vill läsa in header-raden separat, kan du använda "headers: true" som ett argument:

```Ruby
CSV.foreach("filnamn.csv", headers: true) do |rad|
  p rad
end
```

För att skriva data till en CSV-fil, kan du använda "CSV.open" och skicka med filnamn och ett läge, till exempel "w" för att skriva och "a" för att lägga till i en befintlig fil:

```Ruby
CSV.open("ny_fil.csv", "w") do |csv|
  csv << ["Förnamn", "Efternamn"]
  csv << ["Anna", "Svensson"]
  csv << ["Erik", "Andersson"]
end
```

Detta kommer att skapa en ny CSV-fil med för- och efternamn som header och därefter lägga till två nya rader med data. Observera att varje rad måste skrivas som en array.

Om du behöver manipulera data i en befintlig CSV-fil, kan du använda det "CSV.table" för att läsa in hela filen som en tabell och sedan använda vanliga Array-metoder för att manipulera den. Sedan kan du skriva tillbaka datan till filen med "CSV.open" och det läge som passar dina behov.

## Fördjupning

Att arbeta med CSV-filer i Ruby kan innebära en del utmaningar, särskilt om du har stora filer eller behöver utföra avancerade operationer. Därför är det viktigt att också lära sig om andra bibliotek och verktyg som kan hjälpa till med dessa problem. Ett exempel är "FasterCSV" som använder sig av en snabbare parser för stora filer. Det finns också möjlighet att använda "CSV.foreach" på ett mer effektivt sätt genom att använda "each_slice" för att läsa in data i batch.

## Se också

- [Ruby CSV-dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html)
- [FasterCSV på RubyGems.org](https://rubygems.org/gems/fastercsv)
- [Exempel på CSV data](https://github.com/jakebate313/Ruby-CSV-Example-Data)