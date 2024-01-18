---
title:                "Nykyisen päivämäärän hankkiminen."
html_title:           "Lua: Nykyisen päivämäärän hankkiminen."
simple_title:         "Nykyisen päivämäärän hankkiminen."
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän hakeminen tarkoittaa nykyisen päivämäärän ja kellonajan hankkimista tietokoneeltasi. Ohjelmoijat käyttävät tätä tietoa erilaisiin tarkoituksiin, kuten päivämäärän näyttämiseen sovelluksissa, aikaleimojen luomiseen tiedostoille tai osana aikaperusteisia tehtäviä.

## Miten:
```Lua
local current_date = os.date() -- hakee nykyisen päivämäärän ja kellonajan
print(current_date) -- tulostaa tämänhetkisen päivämäärän ja kellonajan
```

```Lua
local current_year = os.date("%Y") -- hakee nykyisen vuoden
print(current_year) -- tulostaa tämänhetkisen vuoden
```

## Syväsukellus:
Päivämäärän hakeminen on tärkeä osa ohjelmointia, koska monet sovellukset ja järjestelmät tarvitsevat tietoa ajasta ja päivämääristä. Nykyisessä Lua-versiossa on käytettävissä os.date() -funktio, joka palauttaa päivämäärän ja kellonajan vakioformaatissa. Vaihtoehtoisesti, jos haluat pidemmän tai tarkemman päivämäärän, voit käyttää os.date() -funktion ensimmäisen parametrinä format-stringiä, joka määrittää, millä tavalla päivämäärä ja kellonaika esitetään.

## Katso myös:
- Lua-os.date dokumentaatio: https://www.lua.org/manual/5.3/manual.html#pdf-os.date