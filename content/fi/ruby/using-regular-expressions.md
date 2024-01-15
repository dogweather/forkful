---
title:                "Säännöllisten ilmeiden käyttö"
html_title:           "Ruby: Säännöllisten ilmeiden käyttö"
simple_title:         "Säännöllisten ilmeiden käyttö"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännöllisten lausekkeiden käyttö on hyödyllistä, kun haluat hakea ja muokata tietoa merkkijonoista tietyllä tavalla. Niitä voidaan käyttää esimerkiksi tietokantojen kyselyissä, tekstinkäsittelyssä ja tiedostonkäsittelyssä.

## Näin käytät säännöllisiä lausekkeita

Säännöllisen lausekkeen käyttö Rubyssa on helppoa. Seuraavassa esimerkissä etsitään merkkijonosta kaikki isot kirjaimet ja tulostetaan ne:

```Ruby
string = "Tämä On Esimerkki"
string.gsub(/[A-Z]+/, '') #=> "lmrmmi"
```

Tässä esimerkissä käytetään `gsub` metodia, joka korvaa kaikki merkkijonossa löytyvät isot kirjaimet tyhjällä merkkijonolla. Säännöllinen lauseke `[A-Z]+` tarkoittaa, että etsitään aakkosten isoja kirjaimia ja `+` merkki tarkoittaa, että etsitään yksi tai useampi esiintymä.

## Syventävä tieto säännöllisistä lausekkeista

Ruby tarjoaa erittäin kehittyneen tavan käyttää säännöllisiä lausekkeita. Voit esimerkiksi käyttää ryhmiä ja kertoa, millä tavoin haluat muokata löytyneitä osia.

Esimerkiksi, kun haluaa vaihtaa merkkijonon sanat paikoillaan, voit käyttää `sub` metodia seuraavasti:

```Ruby
string = "Tämä on esimerkki"
string.gsub(/(\w+) (\w+)/, '\2 \1') #=> "on Tämä esimerkki"
```

Tässä tapauksessa `(\w+) (\w+)` tarkoittaa, että etsitään kaksi sanaryhmää ja ensimmäinen ryhmä näytetään `\1` ja toinen `\2`.

## Katso myös

- [Ruby säännölliset lausekkeet](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Ruby regexp metodi](https://ruby-doc.org/core-2.7.1/Regexp.html#method-i-gsub)
- [Ruby regexp esimerkkejä](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)