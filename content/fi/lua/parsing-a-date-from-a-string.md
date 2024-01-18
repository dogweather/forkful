---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Lua: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa ajan ja päivämäärän tiedon erottamista merkkijonosta ja muuntamista käyttökelpoiseen muotoon. Tätä tehdään usein, jotta ohjelmointikielet ja tietokannat voivat käsitellä päivämääriä ja aikaa helposti ja tarkasti.

Ohjelmoijat suorittavat tämän tehtävän yksinkertaisemman tietojen käsittelyn ja tarkempien laskelmien mahdollistamiseksi.

## Kuinka tehdä:
```
Lua-päivä KirjauduSisään()
  Merkkijono = "2020-10-15"
  Päivämäärä = date.parse(Merkkijono,"%Y-%m-%d")
  Palauta Päivämäärä
End
```
Esimerkiksi yllä olevassa koodissa päivämäärä "2020-10-15" jäsentyy muotoon October 15, 2020.

## Syvemmälle:
Päivämäärän jäsentäminen merkkijonosta ei ole uusi käsite. Ennen tietokoneiden ja ohjelmointikielten kehittämistä, ilman tätä taitoa ihmiset joutuivat laskemaan päivämäärät ja ajat manuaalisesti. On myös muita tapoja jäsentää päivämääriä, kuten käyttämällä erilaisia ​​päivämäärän esitysmuotoja (esimerkiksi päivämäärän kirjoittaminen kirjaimin).

Lua tarjoaa myös muita tapoja jäsentää päivämäärä merkkijonosta, kuten käyttämällä nk. "luettelon muotoilusääntöjä" (list formatting rules). Tämä mahdollistaa päivämäärä- ja ajatietojen eriyttämisen ja muuntamisen haluttuihin muotoihin.

## Katso myös:
- [Lua Reference Manual](https://www.lua.org/manual/5.4/manual.html)
- [Lua-tutoriaali](http://lua-users.org/wiki/TutorialDirectory)
- [Päivämäärän jäsentäminen merkkijonosta Pythonilla](https://realpython.com/python-datetime/)