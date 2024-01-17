---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Ruby: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Päivämäärän muuttaminen merkkijonoksi on yleinen tehtävä ohjelmoinnissa. Sitä tehdään esimerkiksi tietojen tallentamiseksi tietokantaan tai syötteenä käyttäjälle. Ohjelmoijat käyttävät päivämäärän muuttamista merkkijonoksi helpottamaan tietojen käsittelyä ja näytön muotoilua.

## Miten:
Käytä Ruby:n sisäänrakennettua `strftime`-metodia muuttaaksesi päivämäärän merkkijonoksi haluamassasi muodossa. Esimerkkejä:

```Ruby
# Muodosta päivämäärä oletusmuodossa "kuu/päivämäärä/vuosi"
Date.today.strftime("%m/%d/%Y") # => "08/23/2021"

# Näytä vain vuosisata ja vuosi
Date.today.strftime("%Y") # => "2021"

# Näytä päivämäärä täydellisessä tekstimuodossa
Date.today.strftime("%A, %B %d, %Y") # => "Maanantai, Elokuu 23, 2021"
```

## Syväsukellus:
Päivämäärän muuttaminen merkkijonoksi on ollut haaste ohjelmoinnissa, sillä eri kielissä on erilaiset päivämäärän esitystavat. Ruby:n `strftime`-metodi on kehitetty helpottamaan tätä ongelmaa, sillä se hyödyntää C-kielen `strftime`-funktiota, joka osaa käsitellä päivämääräarvoja monipuolisesti.

Vaihtoehtoisesti voit käyttää myös `to_s`-metodia, joka muuttaa päivämäärän merkkijonoksi oletusmuodossa "vuosi-kuukausi-päivä". Voit myös muuttaa päivämäärän muotoa suoraan tulostamalla esimerkiksi `puts Date.today.strftime("%m/%d/%Y")`.

## Katso myös:
- [Ruby:n dokumentaatio strftime-metodista](https://ruby-doc.org/core-3.0.2/Time.html#method-i-strftime)
- [Strftime.net](https://strftime.net/), joka tarjoaa interaktiivisen työkalun Ruby:n `strftime`-muodon syntaksin harjoitteluun.