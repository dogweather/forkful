---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Ruby: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Mitä ja Miksi?

Miksi haluat muuttaa merkkijonon pieniin kirjaimiin? No, se on yksi tapa tehdä merkkijonosta helpommin vertailtavissa oleva. Esimerkiksi, jos vertaat kahta eri sanaa tai lausetta keskenään, on hyvä olla varma, että molemmat ovat samoilla kirjaimilla. Muuten vertailu ei toimi oikein. Monet ohjelmoijat myös muuttavat merkkijonot pieniin kirjaimiin, jotta ei olisi väliä miten käyttäjät syöttävät tekstiä ohjelmaan.

##  Näin teet sen:

```ruby
merkkijono = "Tämä ON esimerkkisana"
p merkkijono.downcase 
```

Tulos: "tämä on esimerkkisana"

Kuten näet, Ruby-kielellä voit käyttää `downcase`-metodia muuttaaksesi merkkijonon pieniin kirjaimiin. Tämä metodi on valmiina osana Rubyä, joten sinun ei tarvitse itse keksiä miten se tehdään.

## Syvällisempi sukellus:

Historiallinen tausta: Merkkijonojen muuttaminen pieniin kirjaimiin on ollut tärkeä ohjelmoinnin käytäntö jo pitkään. Vanhemmissa kielissä tämän toteuttaminen oli monimutkaisempaa, mutta Ruby-kielellä se on erittäin helppoa.

Vaihtoehtoja: Yksi vaihtoehto merkkijonojen käsittelyyn on käyttää `capitalize`-metodia, joka muuttaa vain merkkijonon ensimmäisen kirjaimen isoksi. Tämä voi olla hyödyllistä esimerkiksi nimien käsittelyssä.

Toteutus: Ruby-käyttää Unicode-algoritmeja muuttaakseen merkkijonot pieniin kirjaimiin. Tämä tarkoittaa, että se toimii hyvin kaikenlaisten merkkien kanssa, kuten aksentteja sisältävien kirjainten tai erikoismerkkien kanssa.

## Katso myös:

- [Ruby Language Documentation - String Class](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)
- [Ruby Guides - String Basics](https://www.rubyguides.com/2018/10/ruby-string-methods/)
- [Ruby Programming Language - Official Website](https://www.ruby-lang.org/fi/)