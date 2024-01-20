---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---
title: "Poistaminen merkkejä, jotka vastaavat mallia Ruby:n avulla"
---

## Mikä & Miksi?
Merkkijonosta merkkien poistaminen tarkoittaa tiettyyn kuviin sopivien merkkien poistamista. Tätä tehdään lukemisen tai sopivan tietojen poiminnan helpottamiseksi. 

## Kuinka tehdä:
Ruby sisältää tehokkaan `.delete` -metodin merkkien poistamiseen. 
```Ruby 
  str = "Hei, Maailma!" 
  str.delete 'a' # => "Hei, Milm!" 
```
Ylemmässä esimerkissä poistamme kaikki 'a' -kirjaimet merkkijonosta. Tuloksena olemme saaneet merkkijonon, jossa 'a':ta ei ole. 

Voit myös poistaa useita merkkejä kerralla:
```Ruby 
  str = "Ruby on hieno"
  str.delete 'Ruo' # => "by n hien"
```
Tässä 'R', 'u' ja 'o' kirjaimet on poistettu merkkijonosta.

## Syvällinen tieto:
Historiallisesti metodi `.delete` on ollut Ruby:n standardikirjastossa heti sen ensimmäisen version julkaisusta lähtien, mikä kertoo sen tärkeydestä. 

Vaihtoehtoinen tapa suorittaa sama tehtävä on käyttää `.gsub` -metodia säännöllisten lausekkeiden kanssa. Tämä on kuitenkin monimutkaisempi ja hitaampi menetelmä.

Metodi `.delete` toimii skannaamalla merkkijonon läpi ja poistamalla kaikki määrätyn kuvion mukaiset merkit. Tämä tekee siitä tehokkaan työkalun, etenkin suurien tietomäärien käsittelyssä.

## Katso myös:
Lisätietoja Ruby:n`.delete` -metodista löydät Ruby:n virallisesta dokumentaatiosta. 

[Ruby Doc: String#delete](https://ruby-doc.org/core-2.7.3/String.html#method-i-delete)

Muista tämä ei ole ainoa metodi merkkijonojen manipulointiin Ruby:ssä, voit tutustua myös `.gsub` metodiin ja muihin merkkijonojen käsittelyyn tarkoitettuihin metodeihin.

[Ruby Doc: String#gsub](https://ruby-doc.org/core-2.7.3/String.html#method-i-gsub)