---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:16:22.572257-07:00
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Tietää päivämäärän merkitys: saada tietää juuri nyt vallitseva päivämäärä. Ohjelmoijat tarvitsevat tätä aikaleimojen luomiseen, päivämäärien vertailuun ja aikataulutukseen.

## How to:
```
Ruby
require 'date'

# Haetaan nykyinen päivämäärä
today = Date.today

# Tulostetaan se
puts today
```
Tulostus:
```
2023-04-05
```

## Deep Dive
Syväsukellus: Rubyssa päivämäärän saaminen on yksinkertaista, kiitos sisäänrakennetun Date-luokan. Historiallisesti tätä toiminnallisuutta on käytetty alusta lähtien, koska päivämäärien käsittely on yleinen tarve ohjelmoinnissa. Vaihtoehtoisesti voit käyttää Time-luokkaa, jos tarvitset sekunteja ja aikavyöhykkeitä. Toteutuksessa Date.today kutsuu Date::civil-metodia, joka palauttaa civil-päivämäärän tämänhetkisen gregoriaanisen kalenterin mukaisesti.

## See Also
Lähteet ja lisätietoa:

- Ruby Date-dokumentaatio: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- Time-luokka: [https://ruby-doc.org/core-3.0.0/Time.html](https://ruby-doc.org/core-3.0.0/Time.html)
- Ymmärtääkseen aikavyöhykkeet Rubyssa: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html) (Rails-kehys tarvitaan)
