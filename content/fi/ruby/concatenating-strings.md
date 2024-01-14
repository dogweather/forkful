---
title:                "Ruby: Merkkijonojen yhdistäminen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Yksi tärkeimmistä taidoista, joita aloittelijan tulisi oppia Ruby-ohjelmoidessa, on merkkijonojen yhdistäminen. Merkkijonot ovat yksi ohjelmoinnin peruskäsitteistä ja niitä käytetään usein viestien, käyttäjän syötteiden ja palvelimelta saadun datan käsittelyssä. Merkkijonon yhdistäminen mahdollistaa useiden eri arvojen yhdistämisen yhdeksi kokonaisuudeksi, mikä on erittäin hyödyllistä ohjelmoinnissa.

## Miten

Ruby tarjoaa helpon tavan yhdistää merkkijonoja käyttämällä "+" -merkkiä. Voit yhdistää kaksi merkkijonoa yksinkertaisesti kirjoittamalla ne peräkkäin:

```Ruby
echo = "Hei"
nimi = "Maija"
puts echo + nimi
```

Tämä koodi tulostaa "Hei Maija". Voit myös käyttää yhdistettäessä muuttujia, kuten tässä esimerkissä:

```Ruby
etunimi = "Maija "
sukunimi = "Meikäläinen"
kokonimi = etunimi + sukunimi
puts "Tervehdys, #{kokonimi}!"
```

Tämä koodi tulostaa "Tervehdys, Maija Meikäläinen!". Huomaa, että käytimme "puts"-metodia tulostaaksemme lopputuloksen.

## Syvemmälle

Merkkijonojen yhdistämisessä voi käyttää myös muita merkkejä, kuten "-" tai "*". Ruby tarjoaa myös erityisen "concat" -metodin merkkijonojen yhdistämiseen. Voit lukea lisää tästä ja muista merkkijonojen manipulointitoiminnoista Ruby-oppaasta.

## Katso myös

- [Ruby-oppaat](https://www.ruby-lang.org/fi/documentation/)
- [Merkkijonon yhdistäminen Rubyssa](http://ruby-doc.org/core-2.5.1/String.html#method-i-2B)
- [Merkkijonon muotoilu Rubyssa](https://www.tutorialspoint.com/ruby/ruby_strings.htm)