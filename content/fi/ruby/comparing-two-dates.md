---
title:                "Kahden päivämäärän vertailu"
html_title:           "Ruby: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

##

Miksi: Miksi joku haluaisi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää voit tarkistaa, ovatko ne samat tai kuinka paljon ne poikkeavat toisistaan. Tämä voi olla hyödyllistä esimerkiksi laskutusta tai tehtäväaikojen tarkistamista varten.

## Miten:

Vertaileminen kahden päivämäärän välillä Ruby-ohjelmoinnissa on helppoa. Käytä täsmälleen samaa syntaksia kuin vertailemalla kahta numeroa. 

```
Ruby
require 'date'

date1 = Date.parse("2019-01-01")
date2 = Date.parse("2019-01-15")

if date1 < date2
  puts "Ensimmäinen päivämäärä on ennen toista."
elsif date1 > date2
  puts "Ensimmäinen päivämäärä on jälkeen toisen."
else
  puts "Päivämäärät ovat samat."
end

```

Tulostus:

```
Ensimmäinen päivämäärä on ennen toista.
```

## Syvempi sukellus:

Vertailemalla päivämääriä, Ruby vertaa todellisuudessa päivämäärille annettavia arvoja. Tämä tarkoittaa, että jos päivämäärät eivät ole samassa muodossa, tulos voi olla yllättävä.

```
Ruby
require 'date'

date1 = Date.parse("6.12.2019")
date2 = Date.parse("2019-12-06")

if date1 == date2
  # Tämä kohta ei koskaan suoriteta, koska päivämäärät eivät ole samassa muodossa.
  puts "Päivämäärät ovat samat."
end

```

Tässä tapauksessa tulos on "ArgumentError: invalid date". Tämä johtuu siitä, että ensimmäinen päivämäärä on annettu päivä-kuukausi-vuosi -muodossa, kun taas toinen päivämäärä on vuosi-kuukausi-päivä -muodossa.

## Katso myös:

- [Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/Date.html)
- [Time-luokan dokumentaatio](https://ruby-doc.org/core-2.5.3/Time.html)
- [Ruby Date and Time -opetusvideo](https://www.youtube.com/watch?v=n3fCNW_xAhc)