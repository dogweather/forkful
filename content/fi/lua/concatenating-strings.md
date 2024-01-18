---
title:                "Yhdistetään merkkijonoja"
html_title:           "Lua: Yhdistetään merkkijonoja"
simple_title:         "Yhdistetään merkkijonoja"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

## Mitä se on?
Merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi.

## Miksi kehittäjät tekevät sitä?
Merkkijonojen yhdistäminen on hyödyllistä, koska se mahdollistaa usean merkkijonon yhdistämisen yhdeksi helpommin käsiteltäväksi kokonaisuudeksi. Tämä saattaa olla tarpeellista esimerkiksi silloin, kun halutaan luoda tekstipohjainen käyttöliittymä tai tallentaa käyttäjän antamat tiedot tietokantaan.

## Kuinka tehdä?
```Lua
local etunimi = "Matti"
local sukunimi = "Meikäläinen"
local kokonimi = etunimi .. " " .. sukunimi
print(kokonimi)
```
Tulostus:
```
Matti Meikäläinen
```

## Syväsukellus
### Historiallinen konteksti
Merkkijonojen yhdistäminen oli alunperin kehitetty C-kielen yhteydessä. Myöhemmin se on yleistynyt monilla muillakin kielillä, kuten Lualla.

### Vaihtoehtoiset tavat
Merkkijonon yhdistämisen lisäksi voit myös käyttää table.concat() -funktiota, joka yhdistää taulukon arvot yhdeksi merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun sinulla on paljon erilaisia merkkijonoja, joita haluat yhdistää.

### Toteutus yksityiskohtia
Luan tavoin monet muut kielet käyttävät ".." -operaattoria merkkijonojen yhdistämiseen. Tässä käytännössä on kuitenkin yksi haittapuoli: mikäli yhdistettävien merkkijonojen joukossa on numeroita, ne tulkitaan ensin numeerisiksi arvoiksi ja vasta sen jälkeen muutetaan merkkijonoiksi.

## Katso myös
- [Tietoa merkkijonoista Luan virallisessa dokumentaatiossa](https://www.lua.org/manual/5.1/manual.html#5.4)
- [Esimerkkejä merkkijonojen yhdistämisestä eri ohjelmointikielillä](https://rosettacode.org/wiki/String_concatenation)