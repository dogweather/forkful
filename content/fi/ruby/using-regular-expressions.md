---
title:    "Ruby: Säännöllisten lausekkeiden käyttö"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Ruby-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja Ruby-ohjelmoijille, koska ne mahdollistavat tekstin käsittelyn ja muokkaamisen tehokkaasti koodin avulla. Ne ovat erityisen hyödyllisiä, kun halutaan tarkistaa, onko teksti halutun muotoinen tai etsiä tietyt osiot tekstistä.

## Kuinka käyttää säännöllisiä lausekkeita Rubyssa?

Säännölliset lausekkeet voidaan luoda Rubyssa käyttämällä RegExp-luokkaa. Käytännössä säännöllinen lauseke on merkkijono, joka sisältää erityisiä merkkejä ja merkkijonoja, jotka määrittelevät haetun kuvion. Seuraavassa esimerkissä käytetään säännöllistä lauseketta tarkistamaan, onko sana "kissa" läsnä annetussa lauseessa:

```Ruby
# luodaan RegExp-olio ja tallennetaan se muuttujaan 'regex'
regex = /kissa/

# tarkistetaan, onko lauseessa esiintyy sana 'kissa'
puts regex.match("Olen nähnyt kissan.") # tulostaa 'kissa'
puts regex.match("Hänellä on koira.") # ei tulosta mitään
```

Tässä esimerkissä luodaan säännöllinen lauseke, joka tarkistaa, onko sana "kissa" läsnä annetussa merkkijonossa. Jos sana löytyy, `match`-metodi tulostaa sen, muuten se ei tulosta mitään. Tämä on yksinkertainen esimerkki, mutta säännöllisiä lausekkeita voidaan käyttää monimutkaisempiin kuvioihin, kuten puhelinnumeron tai sähköpostiosoitteen tunnistamiseen.

## Syventävää tietoa säännöllisistä lausekkeista

Säännöllisiä lausekkeita on syvällisemmin useissa eri muodoissa, ja niitä voi käyttää monella eri tavalla. Niiden avulla voidaan esimerkiksi korvata tekstiä tai erottaa osioita tekstistä. Säännöllisiä lausekkeita voi myös optimoida muuttamalla niiden rakennetta tai käyttämällä erilaisia määreitä, kuten `case insensitive` tai `global`.

Toinen hyödyllinen asia säännöllisissä lausekkeissa on niiden yhdistäminen Ruby-merkkijonojen kanssa, jolloin saadaan luotua monimutkaisempia kuvioita. Lisäksi on olemassa useita hyödyllisiä kirjastoja, jotka tarjoavat valmiita säännöllisiä lausekkeita erilaisiin käyttötarkoituksiin.

## Katso myös

* [Ruby-säännöllisten lausekkeiden opas](https://ruby-doc.org/core-2.7.1/Regexp.html)
* [Ruby-merkkijonot](https://ruby-doc.org/core-2.7.1/String.html)
* [Ruby-säännöllisiä lausekkeita hyödyntävät kirjastot](https://rubygems.org/search?query=regexp)