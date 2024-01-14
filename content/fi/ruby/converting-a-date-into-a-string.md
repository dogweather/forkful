---
title:    "Ruby: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi muuttaa päivämäärä merkkijonoksi? Tämä on hyvä kysymys ja siihen on useita vastauksia. Yksi syy voi olla, että haluat näyttää päivämäärän tietyn muodon mukaisesti, kuten "dd/mm/YYYY" tai "mm/dd/YYYY". Tai ehkä haluat tallentaa päivämäärän merkkijonona tietokantaan tai käyttää sitä verkkosivustolla. Luoessaan päivämäärä-olioita, Ruby tarjoaa useita tapoja muuttaa ne merkkijonoiksi, ja tämä blogikirjoitus opettaa sinulle, kuinka se tehdään.

## Kuinka tehdä

Muuttaaksesi päivämäärän merkkijonoksi, sinun täytyy ensin luoda päivämäärä-olio. Tämän jälkeen voit käyttää strftime-menetelmää muuttaaksesi päivämäärän haluamaasi muotoon. Esimerkiksi, jos haluat muuttaa päivämäärän muotoon "dd/mm/YYYY", voit käyttää seuraavaa koodia:

```ruby
date = Date.today # luo päivämäärä-olion tämän päivän päivämäärällä
p date.strftime('%d/%m/%Y') # tulostaa päivämäärän merkkijonona "23/04/2021"
```

Katsotaanpa hieman tarkemmin strftime-menetelmää. Se ottaa yhden argumentin, joka on muotoilumuuttuja, ja palauttaa päivämäärän tässä muodossa. Voit käyttää erilaisia muotoilumuuttujia muuttaaksesi päivämäärää haluamallasi tavalla. Alla on esimerkkejä joistakin yleisimmistä muotoilumuuttujista ja niiden tuottamista tuloksista:

* `%d` - päivän numero (esim. 23)
* `%m` - kuukauden numero (esim. 04)
* `%Y` - vuoden numero (esim. 2021)
* `%b` - lyhyt kuukauden nimi (esim. Apr)
* `%B` - pitkä kuukauden nimi (esim. April)
* `%a` - lyhyt päivän nimi (esim. Fri)
* `%A` - pitkä päivän nimi (esim. Friday)

Voit käyttää myös muita muotoilumuuttujia ja yhdistellä niitä haluamallasi tavalla. Kokeile rohkeasti erilaisia vaihtoehtoja ja löydä juuri sinulle sopiva muotoilu.

## Syvä Sukellus

Kuten jo mainitsimme, Ruby tarjoaa useita tapoja muuttaa päivämäärä-olioita merkkijonoiksi. Yksi vaihtoehto on käyttää to_s-menetelmää, joka palauttaa päivämäärän merkkijonona oletusmuodossa (esim. "2021-04-23"). Kuitenkin tämä menetelmä ei anna sinulle mahdollisuutta muuttaa päivämäärän muotoa. Toisaalta strftime-menetelmä antaa sinulle täyden hallinnan päivämäärän muotoiluun, mutta se vaatii hieman enemmän työtä.

## Katso myös

* Ruby:n virallinen strftime-dokumentaatio: https://ruby-doc.org/core-2.7.0/Time.html#method-i-strftime
* strftime-cheatsheet: https://apidock.com/ruby/DateTime/strftime