---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän parsiminen merkkijonosta on prosessi, jossa puramme ja tulkitaan päivämäärä kirjallisesta muodosta sen numeeriseen esitysmuotoon. Ohjelmoijat tekevät tämän tiedon käsittelyn helpottamiseksi ja päivämäärätietojen käytön mahdollistamiseksi ohjelmissaan.

## Näin se tehdään:

```Lua
-- Luo ensin päivämäärän desimaalimuotoinen esitys merkkijonosta
pvm = os.date("*t", os.time({year=2021, month=11, day=23}))

-- tulostetaan päivämäärä
print(os.date("%x", os.time(pvm)))
```

Tämän koodin ajamisen tuloksena saadaan seuraava:

```Lua
23/11/2021
```

## Syvempi perehtyminen:

Historiallisesti päivämäärän parsiminen merkkijonosta on ollut välttämätöntä aikojen tiedonvälityksessä ja -käsittelyssä lukuisissa sovelluksissa, esimerkiksi logien analysointi ohjelmistojen virheenkorjauksessa. 

Vaikka Lua tarjoaa yllä esitetyn funktion, on monia muitakin tapoja suorittaa tämä toiminto. Esimerkiksi käyttämällä luetteloita, regexejä tai muita kirjastoja, kuten DateLib.

Päivämäärän parsiminen on luonteeltaan yksinkertainen operaatio, mutta sillä on omat sudenkuoppinsa. Lukuisia päivämäärämuotoja ja -standardeja (kuten ISO 8601) huomioonottaen, parsiminen voi olla haaste jos ei käytä oikeita työkaluja tai funktioita.

## Katso myös:

Lisätietoa ohjelmistokehityksestä Lua: 
- [Lua manual](https://www.lua.org/manual/5.4/)
- [Lua.org](https://www.lua.org/)
- [Date and Time in Lua](http://lua-users.org/wiki/DateAndTime)