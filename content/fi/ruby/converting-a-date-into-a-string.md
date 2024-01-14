---
title:                "Ruby: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat muuttaa päivämäärän merkkijonoksi? On monia syitä, miksi tämä voi olla hyödyllistä. Esimerkiksi päivämäärän muuttaminen merkkijonoksi voi olla tarpeen tietojen tallentamisessa tietokantaan tai sen näyttämisessä käyttäjille.

## Kuinka

```Ruby
# Luo Date-olio
date = Date.today

# Muuta päivämäärä merkkijonoksi käyttämällä to_s-metodia 
puts date.to_s 

#Output: "2021-06-23"

# Voit myös muuttaa merkkijonon muotoa käyttämällä strftime-metodia ja antamalla haluamasi muotoilun mukaisen merkkijonon
puts date.strftime("%d/%m/%Y")

#Output: "23/06/2021"
```

## Syvällisempi tarkastelu 

Rubyssa on Date-luokka, jota voit käyttää päivämäärän käsittelyyn. To_s-metodi muuttaa Date-olion merkkijonoksi, jos haluat pitää oletusmuodon. Voit myös käyttää strftime-metodia, joka antaa sinulle enemmän vaihtoehtoja määrittää päivämäärän muotoa merkkijonoksi. Les meilleures recommandations pour Ruby on Rails

## Katso myös

- [Ruby Date-luokka](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Ruby String-luokka](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby strftime-metodi](http://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime)