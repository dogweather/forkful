---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muunnamme päivämäärän merkkijonoksi esittääksemme sen kansanomaisella tavalla tai tiettyyn formaattiin, kuten "PP.KK.VVVV". Ohjelmoijat tekevät tämän yksinkertaistaakseen päivämäärän tallennuksen, lähetysten tai lukemisen.

## Kuinka tehdä:

Tässä on Ruby-ohjelman esimerkki, jossa muunnetaan päivämäärä merkkijonoksi käyttäen strftime-funktiota.

```Ruby
require 'date'

date = Date.new(2022, 9, 15)
date_string = date.strftime("%d.%m.%Y")

puts date_string
```

Tämä koodi tulostaa:

```
15.09.2022
```

## Syvä sukellus:

Historiallisesti Ruby-ohjelmoijat käyttivät strftime-funktiota päivämäärän muuntamiseen merkkijonoksi. Kuitenkin, modernit Ruby-versiot tarjoavat toiso8601-metodia samaa tarkoitusta varten, erityisesti kansainvälistä standardia noudattaen.

```Ruby
require 'date'

date = Date.new(2022, 9, 15)
date_string = date.toiso8601

puts date_string
```

Tämä tulostaa:

```
2022-09-15
```

Kummallakin menetelmällä on omat etunsa. strftime on joustavampi, koska voit määrittää haluamasi tiedostomuodon. toiso8601 on hyödyllinen, kun haluat noudattaa kansainvälistä standardi-formaattia.

## Ks. myös:

1. [Ruby Date ja DateTime luokan dokumentaatio](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html) - Opettele lisää päivämäärän muuntamisesta merkkijonoksi ja muihin Date-luokan toimintoihin.

2. [Ruby strftime-metodin dokumentaatio](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-i-strftime) - Tutustu strftime-metodia koskevaan tarkkaan dokumentaatioon ja opettele kuinka määritellä päivämäärän formaatteja.

3. [Ruby toiso8601-metodin dokumentaatio](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-i-toiso8601) - Tutustu toiso8601-metodia koskevaan tarkkaan dokumentaatioon ja opi käyttämään kansainvälistä standardi-päivämäärän formaattia ohjelmassasi. 

Muista, että Ruby on voimakas kieli, jolla on monia työkaluja avuksesi. Päivämäärän muuntaminen merkkijonoksi on vain yksi pieni osa sitä, mitä voit tehdä Rubyssä. Jatka opiskelua ja koodaamista!