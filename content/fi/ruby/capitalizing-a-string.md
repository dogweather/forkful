---
title:                "Merkkijonon isompi kirjoitusasu"
html_title:           "Ruby: Merkkijonon isompi kirjoitusasu"
simple_title:         "Merkkijonon isompi kirjoitusasu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Miksi käyttäjät käyttävät rubyä ja mitä tapahtuu, kun merkkijono kirjoitetaan isoilla kirjaimilla.

## Kuinka tehdä?

Tässä on pieni koodiesimerkki, joka näyttää kuinka kirjoittaa merkkijono isoilla kirjaimilla rubyssä:

```ruby
puts "moikka".upcase
```

Tämä koodi tulostaa "MOIKKA" terminaaliin.

## Syvällinen sukellus

Pääkirjaston kirjailija James A. Duncan kertoo, että aikaisemmissa ohjelmistoversioissa käyttäjien piti käyttää itse kirjastoa merkkijonojen muotoiluun, mikä oli melko hankalaa. Siksi pääkirjaston päivitys sisältää nyt kätevän metodin `.upcase`, joka helpottaa merkkijonojen muotoilua.

On myös olemassa vaihtoehtoisia tapoja kirjoittaa merkkijono isoilla kirjaimilla, kuten `.capitalize`-metodi, joka muuttaa vain merkkijonon ensimmäisen kirjaimen isoksi.

## Katso myös

Voit lukea lisää merkkijonojen muotoilusta Rubyssa [Ruby-dokumentaatiosta](https://ruby-doc.org/core-3.0.0/String.html#method-i-upcase).