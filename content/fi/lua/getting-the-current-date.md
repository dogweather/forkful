---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?
Hankinta nykyinen päivämäärä on operaatio, jolla saadaan tietää päivämäärä reaaliajassa. Ohjelmoijat tekevät tämän, jotta ylläpitäisivät tarkkoja lokitietoja tai seurata ajankohtaisia tapahtumia.

# Kuinka tehdä:
Tässä on perus esimerkki koodi miten saada nykyinen päivämäärä Lua:ssa.

```Lua
os.date()
```

Koska tämä koodi suoritetaan, se palauttaa nykyisen päivämäärän ja ajan merkkijono.

```Lua
print(os.date())
```

Tämä tulostaa jotain tämän kaltaisaa: 

```Lua
"Tue Jun 12 14:53:11 2022"
```

# Syvä sukellus

1. Historiallinen konteksti: Lua on ollut olemassa vuodesta 1993, ja os.date() funktio on ollut mukana alusta asti. Sen toimintaperiaate on osana Os-kirjastoa, joka tarjoaa yleisiä toimintoja käyttöjärjestelmälle.

2. Vaihtoehdot: Joskus saatat haluavat muokata päivämäärän ulkoasua. Seuraavassa esimerkissä näytämme kuinka näyttää päivämäärä eurooppalaisessa muodossa (PP.KK.VVVV).

```Lua
print(os.date("%d.%m.%Y"))
```

3. Toteutuksen yksityiskohdat: `os.date()` toimii kutsuen C-kirjaston `strftime()` funktiota, joka muotoilee ajan merkkijonoksi. Se ottaa vapaaehtoisena argumenttina ajan esitysmuodon. 

# Katso myös
Jos haluat oppia lisää Lua:n ajankäsittelystä, tutustu näihin lähteisiin:
   
   1. [Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/)
   
   2. [Programming in Lua, Fourth Edition](http://www.lua.org/pil/)
   
   3. [Lua-Users Wiki: Dates And Time](http://lua-users.org/wiki/DatesAndTime)
   
   4. [Stack Overflow questions tagged 'Lua'](https://stackoverflow.com/questions/tagged/lua)