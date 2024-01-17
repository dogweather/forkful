---
title:                "Att arbeta med csv"
html_title:           "Ruby: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Working med CSV står för "Comma Separated Values" och det är ett vanligt filformat för att lagra tabulära data, som ofta används för att överföra data mellan olika program och system. Programmers arbetar med CSV för att enkelt kunna läsa, manipulera och analysera data från olika källor.

## Hur man gör:
Här är ett enkelt exempel på hur man kan använda Ruby för att läsa och skriva till en CSV-fil:

```Ruby
require 'csv'

# Läsa in en CSV-fil
CSV.foreach("exempel.csv") do |row|
  puts row
end

# Skriva till en CSV-fil
CSV.open("ny_film.csv", "w") do |csv|
  csv << ["Titel", "Regissör", "År"]
  csv << ["The Dark Knight", "Christopher Nolan", 2008]
end
```

Output för ovanstående kod skulle vara:
```Ruby
["Titel", "Regissör", "År"]
["The Dark Knight", "Christopher Nolan", 2008]
```

## Djupdykning:
~Historisk kontext: CSV-filer har funnits sedan 1972 och utvecklades från ett enkelt kommaavgränsat dataformat. De användes initialt främst för datautbyte mellan olika databassystem men har senare blivit ett vanligt sätt att lagra och överföra data mellan olika program.

~Alternativ: Det finns många andra filformat för tabulära data, som t.ex. JSON och XML, men CSV är fortfarande populärt på grund av dess enkelhet och läsbarhet.

~Implementering: Ruby har en inbyggd CSV-modul som gör det enkelt att läsa och skriva till CSV-filer. Det finns också många tredjepartsbibliotek som erbjuder mer avancerade funktioner för hantering av CSV-data.

## Se även:
- [Ruby's CSV-dokumentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [Ruby CSV-gem](https://rubygems.org/gems/csv)
- [En tutorial om att arbeta med CSV i Ruby](https://www.rubyguides.com/2018/10/parse-csv-ruby/)