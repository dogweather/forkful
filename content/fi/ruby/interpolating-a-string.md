---
title:                "Merkkijonon interpolointi"
html_title:           "Ruby: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

Miellyttävää lukemista!

## Mitä ja miksi?
Interpolointi tarkoittaa merkkijonon sisällön korvaamista muuttujien arvoilla. Tämä tehdään yleensä siksi, että halutaan luoda dynaamisia ja muuttuvia merkkijonoja. Esimerkiksi käyttäjänimien, terran lajien tai muuttuvien tietojen lisääminen merkkijonoon.

## Miten:
```Ruby
# Luo muuttuja
name = "Marika"

# Interpolointi
puts "Hei #{name}, tervetuloa!"

# Tuloste: Hei Marika, tervetuloa!
```
Interpolointi tehdään käyttämällä ```#{ }``` kaarisulkeita ja sijoittamalla sulkujen väliin muuttujan nimi. Tulosteen sijaan interpoloitu merkkijono voidaan myös tallentaa muuttujaan.

## Syvällinen sukellus
Interpolointi sai alkunsa Rubyssa vuonna 1994 ja siitä tuli nopeasti suosittu tapa luoda dynaamisia merkkijonoja. Monilla muilla kielillä on myös vastaavia toimintoja, kuten C:n ```sprintf``` ja PHP:n ```printf```. Nämä toiminnot voivat olla hyödyllisiä silloin, kun halutaan rajata muuttujien tyyppiä tai lisätä erityyppisiä muuttujia, kuten desimaalilukuja.

## Katso myös
* [Ruby:n virallinen dokumentaatio](https://www.ruby-lang.org/en/documentation/)
* [Ruby Monk - Interpolation](https://rubymonk.com/learning/books/1-ruby-primer/chapters/42-strings/lessons/86-string-interpolation)