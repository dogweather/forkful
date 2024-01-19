---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muuntaminen päivämäärästä merkkijonoksi on prosessi, jossa päivämääräobjekti muunnetaan merkkijonoksi miellyttävässä tai tietyn standardin mukaisessa muodossa. Käytännössä tämä auttaa kehittäjiä esittämään päivämäärän käyttäjälle helposti ymmärrettävässä muodossa tai tallettamaan sen tietokantaan.

## Kuinka tehdä:

Alla on esimerkki siitä, kuinka muunnos tehdään Lua-ohjelmointikielessä. 

```Lua
os.setlocale("fi_FI") -- Set the locale to Finnish

t = os.date('*t')  -- get the date as a table
s = os.date('%c',os.time(t))  -- format the time
print(s)
```

Lähdekoodin suorittamisen tulos voi olla jotakin alla olevan kaltaista:

```Lua
"ke 19 tammi 2022 14:58:10"
```

## Syvällisemmin:

Useissa ohjelmointikielissä, mukaan lukien Lua, päivämäärien käsittelyllä on pitkä historia. Päivämäärän muuttaminen merkkijonoksi on yleinen toiminta, joka voi vaihdella eri kielien ja alustojen välillä.

On olemassa useita vaihtoehtoja päivämäärän käsittelyyn. Lua valitsee `os.date` -funktion, jonka avulla voit määrittää oman muotoilusi päivämäärän esittämiseen. Tämä tarkoittaa, että voit olla joustava päivämäärän esittämistavassa. 

Muunnos toteutetaan käyttämällä C:n `strftime` -funktiota taustalla. Tämä toiminta ei ole pelkästään tehokas, vaan se myös hyödyntää alustan olemassa olevia ominaisuuksia, jolloin koodin suorituskyky on parempi.

## Katso myös:

1. Lua 'os.date' dokumentaatio: https://www.lua.org/pil/22.1.html
2. strftime-funktion dokumentaatio: https://man7.org/linux/man-pages/man3/strftime.3.html
3. Lisätietoja päivämäärän ja ajan esittämisestä: https://www.lua.org/pil/22.1.html