---
date: 2024-01-26 03:46:37.786309-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.423475-06:00'
model: gpt-4-0125-preview
summary: .
title: Avrundning av tal
weight: 13
---

## Hur man gör:
```Ruby
# Grundläggande avrundning
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Ange precision
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Avrunda nedåt
puts 2.9.floor          # => 2

# Avrunda uppåt
puts 2.1.ceil           # => 3

# Avrunda mot noll
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Exempel på utskrift:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Fördjupning
Att avrunda tal är inte nytt—människor har gjort det i århundraden för att göra beräkningar enklare eller för att arbeta inom begränsningarna av deras verktyg. I Ruby är `round`-metoden mångsidig, med förmågan att avrunda till närmaste heltal som standard eller till en angiven decimalplats.

Ett alternativ till `round` är `floor` för att alltid avrunda nedåt, och `ceil` för att alltid avrunda uppåt, oavsett talets värde. För att bara kapa av decimalplatserna har du `truncate`.

Historiskt sett, när det kommer till datorer, blir avrundning kritiskt i hantering av flyttalsaritmetik på grund av dess inneboende oprecision. Ruby, liksom de flesta språk, följer IEEE 754-standard för flyttal, vilket innebär att det hanterar avrundning på ett sätt som de flesta programmerare bör kunna förutse och förlita sig på.

Det finns dock mer att det—saker som bankavrundning (även känd som avrundning till närmaste jämnt tal) är koncept som Ruby-utvecklare kan behöva manuellt implementera, eftersom `round`-metoden inte erbjuder det direkt.

## Se även
- [Ruby-dokumentationen](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) för Floats `round`-metod.
- [IEEE-standard för flyttalsaritmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Förstå flyttalens precision](https://floating-point-gui.de/), för en djupare inblick i hur datorer hanterar decimaltal.
