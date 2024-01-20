---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:15:38.643060-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi? Päivämäärän hakeminen tarkoittaa senhetkisen päivän ja ajan selvittämistä. Ohjelmoijat käyttävät tätä toimintaa aikaleimojen luomiseen, tapahtumien ajoitukseen ja käyttäjäkokemuksen personointiin.

## How to:
Miten tehdään:

```Lua
-- Nykyisen päivän ja ajan hankkiminen
local pvm = os.date("*t")  -- hakee nykyisen paikallisen ajan taulukkona

-- Tulostetaan päivämäärä ja kellonaika
print("Päivämäärä:", pvm.year, pvm.month, pvm.day)
print("Kellonaika:", pvm.hour, pvm.min, pvm.sec)

-- Formaattiin sopiva merkkijonoesitys
local aikaleima = os.date("%Y-%m-%d %H:%M:%S")
print("Aikaleima:", aikaleima)
```

Sample output:

```
Päivämäärä: 2023 3 15
Kellonaika: 16 45 22
Aikaleima: 2023-03-15 16:45:22
```

## Deep Dive
Syväsukellus: `os.date`-funktio on Lua-kirjaston osa, joka on peräisin C-kielestä. Se muuntaa ajan merkkijonoksi tai taulukkoksi, joten on helppoa valita joustava esitystapa. Vaihtoehtoisesti `os.time` palauttaa sekunneissa nykyisen Unix-aikaleiman. `os.date`-funktio hyväksyy useita muotoiluja, ja sillä on globaaleja siirtymiä (kuten UTC) ja paikallisia aikoja käsittelevät versiot.

## See Also
Katso myös:
- [Lua 5.4 referenssi](https://www.lua.org/manual/5.4/)
- [Lua-date-funktioon syvemmin](https://www.lua.org/pil/22.1.html)
- [Unix-aikaleiman käsittely](https://en.wikipedia.org/wiki/Unix_time)