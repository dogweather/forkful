---
title:                "Ruby: Omvandla ett datum till en sträng."
simple_title:         "Omvandla ett datum till en sträng."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man skulle vilja konvertera ett datum till en sträng i Ruby. Det kan vara att du vill visa datumet på ett specifikt sätt, göra jämförelser mellan olika datum eller lagra datumen i en databas. Oavsett anledning är det en viktig färdighet att ha när man programmerar i Ruby.

## Så här gör du

För att konvertera ett datum till en sträng i Ruby finns det flera sätt att gå till väga. Det enklaste sättet är att använda den inbyggda metod `strftime`, vilken tar ett datum som input och returnerar en sträng med det specifierade formatet. Se nedan för ett kodexempel:

```Ruby
# Skapa ett datum objekt
today = Date.today

# Konvertera datumet till en sträng med formatet MM/DD/YYYY
puts today.strftime("%m/%d/%Y") #=> "01/22/2021"

# Konvertera datumet till en sträng med formatet DD MMMM YYYY på svenska
puts today.strftime("%d %B %Y") #=> "22 januari 2021"
```

Som du kan se i exemplet ovan kan du använda olika bokstäver för att specifiera olika delar av datumet som ska visas i strängen. Det finns också andra metoder som kan användas för att konvertera datum till strängar, såsom `to_s` eller `to_formatted_s`.

## Djupdykning

För att förstå konverteringen av datum till strängar i Ruby är det viktigt att ha en grundläggande förståelse för `strftime`-metoden. Denna metod tillåter oss att skapa en sträng med ett specifikt format genom att använda bokstäver som representerar olika delar av datumen. Till exempel betyder `%m` att månaden ska visas på två siffror och `%d` betyder att dagen ska visas på två siffror.

För mer information om de olika bokstäverna som kan användas i `strftime`, rekommenderar vi att läsa dokumentationen för Date-klassen i Ruby.

## Se även

- http://ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/Date.html
- https://www.rubyguides.com/2015/06/ruby-date-time/
- https://www.rubyguides.com/2015/06/ruby-time/