---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän jäsentäminen merkkijonosta on prosessi, jossa muutetaan merkkijono päivämääräolion muotoon. Ohjelmoijat tekevät tätä, koska sen avulla he voivat työskennellä päivämäärätietojen kanssa objektisuuntaisen paradigman sisällä.

## Kuinka:

Rubyssa päivämäärien jäsentäminen tehdään `Date`-luokan metodeilla. Katsotaan esimerkkiä:

```ruby
require 'date'

string_date = "2022-04-30"
parsed_date = Date.parse(string_date)

puts parsed_date
```

Tämän koodin ajaessa konsoli tulostaa: `2022-04-30`.

## Syväsukellus:

Historiallisesti päivämäärän jäsentämistarve on syntynyt monimutkaisemman ohjelmoinnin tarpeesta. Aikaisemmin oli yleistä liittää päivämäärätiedot ohjelmakoodiin suoraan, mutta nykyään päivämäärien käsittelyyn on olemassa erilaisia tehokkaita metodeja.

Vaihtoehtoisia tapoja tulkita merkkijono päivämääräksi Rubyssa ovat `Date.strptime` ja `DateTime.parse`. Nämä funktiot voivat olla tehokkaampia, kun tiedät tarkasti merkkijonon muodon, jota käsittelet.

Jäsentämisen toteutus Rubyssa perustuu yleisesti ottaen luokkien ja niiden olio-orientoitujen metodien käyttöön. Ruby käyttää tiukkaa olio-orientoitua suunnittelua, joten jokaisen merkkijonon päivämääräksi jäsentävän funktion on palautettava uusi `Date`-olio.

## Katso Myös:

1. Ruby Date Library Documentation: https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html
2. Tutorial on Parsing Dates: https://www.rubyguides.com/2017/12/ruby-string-format/
3. Deeper Dive into Date Parsing: https://www.justinweiss.com/articles/3-steps-to-better-api-design-using-ruby/