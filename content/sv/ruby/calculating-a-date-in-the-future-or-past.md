---
title:                "Ruby: Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför
Att beräkna ett datum i framtiden eller förflutna kan vara en mycket användbar funktion i många olika situationer. Oavsett om man behöver planera ett möte, boka en resa eller visa en deadline, kan kunskap om hur man beräknar datum vara till hjälp.

## Så här gör du
För att beräkna ett datum i framtiden eller förflutna i Ruby, kan man använda sig av Date-klassen. Det finns olika metoder inom klassen som gör det möjligt att manipulera datumet, till exempel ```+``` för att lägga till dagar eller ```-``` för att ta bort dagar. Här är ett exempel på hur man kan beräkna ett datum 10 dagar framåt i tiden:

```
```Ruby
today = Date.today
future_date = today + 10
puts future_date
# output: 2020-10-26
```

Man kan också använda sig av metoden ```parse``` för att konvertera en sträng med ett datum till ett Date-objekt. Här är ett exempel på hur man kan beräkna ett datum 5 dagar tidigare:

```
```Ruby
today = Date.today
past_date = Date.parse('2020-10-15')
new_date = past_date - 5
puts new_date
# output: 2020-10-10
```

## Djupdykning
När man beräknar datum i Ruby, är det viktigt att förstå hur klassen Date fungerar och vilka metoder som finns tillgängliga. Det finns till exempel möjlighet att jämföra två datum med ```==```, ```>``` eller ```<``` och även att formatera datum i olika format med hjälp av metoden ```strftime```.

En viktig sak att komma ihåg är att Ruby använder sig av den gregorianska kalendern, vilket kan påverka beräkningen av datum beroende på vilket land man befinner sig i. Det är också möjligt att manipulera datum med hjälp av andra klasser, som DateTime och Time, beroende på vilka specifika behov man har.

## Se också
Här är några användbara resurser för att lära sig mer om att beräkna datum i Ruby:

- Ruby Docs - [Date Class](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- Codecademy - [Manipulating and Formatting Dates in Ruby](https://www.codecademy.com/learn/paths/learn-ruby/modules/learn-ruby-date-and-time-u/lessons/date-and-time-u/exercises/manipulating-date)
- RubyGuides - [Ruby Date and Time](https://www.rubyguides.com/2015/10/ruby-datetime-methods/)
- Techtopia - [Manipulating Dates and Times in Ruby](https://www.techotopia.com/index.php/Manipulating_Dates_and_Times_in_Ruby)

Genom att använda dessa resurser och öva på att beräkna datum i Ruby, kan man snart behärska denna användbara funktion och använda den för att underlätta sina uppgifter och projekt.