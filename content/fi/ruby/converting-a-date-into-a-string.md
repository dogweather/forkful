---
title:                "Päivämäärän muuntaminen merkkijonoksi"
aliases:
- fi/ruby/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:47.932423-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Muunnetaan päivämäärä merkkijonoksi, jotta sitä voi käyttää tekstiyhteyksissä, kuten käyttöliittymissä ja raporteissa. Koodarit tekevät tämän esittääkseen päivämääriä ymmärrettävässä muodossa ja suorittaakseen päivämäärien kanssa tehtäviä manipulointeja.

## How to:
```Ruby
require 'date'

# Nykyinen päivämäärä
date_today = Date.today
# Oletusmuotoilu
puts date_today.to_s  # => "2023-04-12"

# Määritetty muotoilu strftime-metodilla
puts date_today.strftime('%d-%m-%Y')  # => "12-04-2023"
puts date_today.strftime('%d/%m/%Y')  # => "12/04/2023"
puts date_today.strftime('%B %d, %Y') # => "April 12, 2023"
```

## Deep Dive:
Päivämäärien muuntaminen merkkijonoksi Rubyssa perustuu `Date` ja `Time` -luokkiin, jotka ovat olleet osa kieltä melkein alusta asti. `strftime`, mikä tulee C-kielen standardikirjaston funktiosta, antaa tavan määritellä päivämäärän esitysmuodon. Historiallisesti tämä on auttanut yhdenmukaistamaan päivämäärien käsittelyä eri ohjelmointikielissä.

Vaihtoehtoiset tapoja ovat esimerkiksi `to_formatted_s`-metodi Rails-frameworkissa tai lisäkirjastot kuten `Chronic` helppoon luonnollisten kielen päivämäärien käsittelyyn. Muotoilun yksityiskohdat, kuten `-`, `/`, tai sanalliset kuukausien nimet (`%B`), riippuvat sovelluksen käyttöyhteydestä.

## See Also:
- Ruby Time-dokumentaatio: [https://ruby-doc.org/core/Time.html](https://ruby-doc.org/core/Time.html)
- strftime()-metodin formaattispecifikaatiot: [http://ruby-doc.org/core-2.5.1/Time.html#method-i-strftime](http://ruby-doc.org/core-2.5.1/Time.html#method-i-strftime)
- Chronic-kirjasto luonnollisen kielen päivämäärille: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
