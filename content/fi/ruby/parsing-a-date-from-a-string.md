---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Ruby: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän parsiminen merkkijonosta on prosessi, jossa muunnetaan tekstiin kirjoitettu päivämäärä tietokoneen ymmärtämäksi muodoksi. Tämä on hyödyllistä, kun haluamme käsitellä päivämäärää ohjelmassamme tai verrata sitä muihin päivämääriin. Se on myös yleinen tehtävä ohjelmoinnissa.

## Kuinka tehdä se:
```Ruby
require 'date'

# Parsi päivämäärä merkkijonosta
date = Date.parse("12.4.2021")
puts date # 2021-04-12

# Muokkaa parsittua päivämäärää
date += 1 # lisää yhden päivän
puts date # 2021-04-13

# Vertaa päivämääriä
date < Date.today # palauttaa true
```

## Syväsukellus:
Päivämäärän parsimisen käsite on ollut osa ohjelmointia jo pitkään, mutta siitä on tullut helpompaa kielen mukana. Aikaisemmin ohjelmoijien täytyi käyttää monimutkaisempia algoritmeja päivämäärien käsittelyyn, mutta nykyään monet ohjelmointikielet, kuten Ruby, tarjoavat sisäänrakennetun Date-luokan, joka tekee tämän prosessin helpommaksi. Vaihtoehtoina päivämäärän parsimiselle ovat esimerkiksi käsin kirjoittaminen tai erilaisten kirjastojen käyttäminen. Rubyssa päivämäärän parsiminen toimii DateTime-luokan avulla, joka perustuu uudempiin standardeihin kuin Date-luokka.

## Katso myös:
- [Ruby Date-luokan dokumentaatio](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Blogipostaus "Päivämäärien parsiminen" Rubyssä](https://www.pluralsight.com/guides/ruby-date-parsing)