---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:12.080967-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng handlar om att konvertera text\
  \ som representerar ett datum till ett `Date` eller `DateTime` objekt som Ruby f\xF6\
  rst\xE5r.\u2026"
lastmod: '2024-03-13T22:44:38.440697-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng handlar om att konvertera text\
  \ som representerar ett datum till ett `Date` eller `DateTime` objekt som Ruby f\xF6\
  rst\xE5r.\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Vad & Varför?
Att tolka ett datum från en sträng handlar om att konvertera text som representerar ett datum till ett `Date` eller `DateTime` objekt som Ruby förstår. Programmerare gör detta för att utföra operationer som jämförelser, beräkningar eller formatering på datum, vilket är vanliga uppgifter i applikationer som hanterar schemaläggning, analys eller databehandling.

## Hur gör man:
I Ruby erbjuder standardbiblioteket direkta sätt att tolka datum från strängar genom att använda `Date` och `DateTime` klasserna. Så här gör du det med Rubys inbyggda metoder:

```ruby
require 'date'

# Tolkar ett datum från en sträng
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime för mer detaljerad tidsrepresentation
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

För mer kontroll eller för att hantera format som `parse` kanske inte direkt förstår, kan du använda `strptime` (sträng tolka tid), genom att explicit ange formatet:

```ruby
# Använda strptime för anpassade format
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Använda tredjepartsbibliotek:

Även om Rubys inbyggda funktioner är kraftfulla, ibland kan du föredra tredjepartsbibliotek för ytterligare funktioner eller enklare syntax. Ett populärt val är `Chronic`-gemet för naturlig språktolkning:

1. Först, lägg till Chronic i din Gemfile och kör `bundle install`:
```ruby
gem 'chronic'
```

2. Använd det sedan så här:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('nästa tisdag')
puts parsed_chronic
# Utdata varierar beroende på det aktuella datumet; antar tolkning den 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` är mycket användbart för användarinmatning eftersom det kan förstå ett brett utbud av naturliga språkdatumformat, vilket gör det till ett kraftfullt verktyg för applikationer som kräver flexibel datumindata.
