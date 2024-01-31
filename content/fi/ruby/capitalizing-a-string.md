---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Isoilla kirjaimilla kirjoittaminen muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Ohjelmoijat käyttävät sitä, kun haluavat korostaa jotain, kuten nimen alun tai otsikon.

## How to: (Kuinka tehdä:)
```Ruby
# Käytä 'capitalize' metodia aloittaaksesi merkkijonon isolla kirjaimella
puts "suomi".capitalize  # Tulostaa "Suomi"

# Ketjuta 'capitalize' toisiin metodeihin muokkauksen laajentamiseksi
puts "hei maailma".split.each(&:capitalize).join(' ')  # Tulostaa "Hei Maailma"

# Rails-kehyksessä 'titleize' metodi voi olla kätevä
puts "hei maailma".titleize  # Tulostaa "Hei Maailma" (vain Railsissa)
```

## Deep Dive (Sukellus syvälle):
String-objektin pääkuori on ollut Rubyssa alusta asti. 'Capitalize'-metodi on suoraviivainen: se tekee ensimmäisestä kirjaimesta ison ja loput pieniksi. Historiallisesti tämä on yksinkertaistanut tekstinkäsittelyä. Vaihtoehtoja sisältävät 'upcase' (muuttaa kaikki kirjaimet isoiksi), 'downcase' (muuttaa kaikki pieniksi), ja 'swapcase' (vaihtaa isoja ja pieniä kirjaimia päittäin).

Rails-kehyksen 'titleize' metodi menee pidemmälle muuttaen jokaisen sanan ensimmäisen kirjaimen isoksi. Se on osa Railsin 'Active Support' -kirjastoa, ei puhtaan Rubyn kielen sisäänrakennettua toiminnallisuutta.

Rubiini toteuttaa merkkijono-operaatiot erittäin tehokkaasti, joten vaikka käsitteletkin suuria tekstimääriä, 'capitalize' ja muut vastaavat metodit toimivat sulavasti.

## See Also (Katso myös):
- Rubyn dokumentaatio String#capitalize -metodista: [ruby-doc.org/core/String.html#method-i-capitalize](https://ruby-doc.org/core/String.html#method-i-capitalize)
- Rails-dokumentaatio String#titleize -metodista Active Support -kirjastosta (vain jos käytät Railsia): [api.rubyonrails.org/classes/String.html#method-i-titleize](https://api.rubyonrails.org/classes/String.html#method-i-titleize)
- Ruby Style Guide, hyvät käytännöt Rubyn kirjoittamiseen: [rubystyle.guide](https://rubystyle.guide)
