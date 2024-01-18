---
title:                "Att tolka ett datum från en sträng"
html_title:           "Ruby: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att "parsa" eller omvandla ett datum från en sträng är processen att extrahera och formatera ett datum från en textsträng, till exempel "1 januari 2021", till ett format som datorn kan förstå och hantera som ett datum. Programmerare gör detta för att kunna behandla datumet på ett meningsfullt sätt i sina program. 

## Så här gör man:

För att parsa ett datum från en sträng i Ruby, kan du använda metoden `Date.parse`. Du behöver bara skapa en ny instans av datumklassen och mata in den sträng som innehåller datumet. Här är ett exempel:

```ruby
date = Date.parse("1 januari 2021")
puts date
```

Outputen från detta program blir: `2021-01-01`

Om du vill specificera ett visst datumformat, kan du ange det som ett argument till `parse` metoden. Här är ett exempel på hur du kan ange formatet "dd/mm/yyyy":

```ruby
date = Date.parse("01/01/2021", "%d/%m/%Y")
puts date
```

Outputen från denna kod skulle bli: `01-01-2021`

## Djupdykning:

Att parsa datum från strängar är en vanlig uppgift för programmerare, särskilt för dem som arbetar med webbapplikationer där datum ofta hämtas från användarinput eller databaser. Det finns också alternativa metoder för att parsa datum i Ruby, såsom att använda bibliotek som `Chronic` eller `Date.strptime`.

När en sträng parsas till ett datum följer Ruby standarden YYYY-MM-DD. Det är också värt att notera att `parse`- metoden inte hanterar ogiltiga datum, till exempel "30 februari", utan kastar istället ett undantag. För att undvika detta kan du använda  `Chronic`- biblioteket som är mer flexibelt när det gäller att parsa ogiltiga datum.

## Se också:

- [Ruby dokumentationen om Date klassen](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Chronic bibliotekets dokumentation](https://github.com/mojombo/chronic)
- [Date.strptime dokumentation](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html#method-c-strptime)