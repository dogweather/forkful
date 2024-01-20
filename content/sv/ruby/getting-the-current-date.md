---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ruby Programmering: Hur man får Nuvarande Datum 

## Vad & Varför?

Att få det nuvarande datumet innebär att extrahera datumdata från datorns systemklocka. Programmerare gör detta för en rad applikationer, som att spåra händelser, generera tidsstämplar eller schemalägga framtida uppgifter.

## Hur man gör: 

I Ruby får vi det nuvarande datumet och tiden genom att använda inbyggda Time, Date och DateTime klasser. 

Här är ett exempel:

```Ruby
require 'date'

date_today = Date.today
puts date_today
```

När du kör detta kodstycke skulle du till exempel få utskriften '2022-11-19'. Det betyder att det idag är den 19:e november 2022.

## Djupdykning

Ruby erbjuder alternativ för att effektivt hantera datum och tid. `Time`-klassen tillhandahåller funktioner för att arbeta med tid och datum. `DateTime` ger liknande funktioner men inkluderar stöd för tidzoner.

Historiskt sett har många språk, inklusive tidigare versioner av Ruby, kämpat med att stödja datumbearbetning, särskilt när det gäller tidzoner. Ruby's moderna klasser är utformade för att anpassas till dessa utmaningar.

För att se mängden sekunder sedan epoken, d.v.s. 1970-01-01 00:00:00 kan man använda `Time#to_i` eller `Time#to_f` metoder:

```Ruby
puts Time.now.to_i
```

`Time`-klassens metoder returnerar värden baserat på systemets tidszon. För att arbeta med andra tidzoner, kan vi använda `DateTime`-klassen.

## Se även:

För mer detaljerad information om Ruby's datum- och tidsklasser, besök de officiella dokumentationssidorna:

- [Time](https://ruby-doc.org/core-2.7.0/Time.html)
- [Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [DateTime](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/DateTime.html)

Att arbeta med datum och tider är en väsentlig del av många program, och förståelse av detta i Ruby vill underlätta för dig att skriva effektiv och korrekt kod.