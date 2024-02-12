---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases:
- /fi/lua/printing-debug-output.md
date:                  2024-01-20T17:53:13.586530-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä se on ja miksi ihmeessä?

Koodin debuggaus eli vianetsintä on prosessi, jossa etsitään ja korjataan ohjelmakoodin virheitä. Tulostamalla diagnostiikkaa konsoliin, ohjelmoijat näkevät nopeasti, mitä koodissa tapahtuu ja missä vika saattaa piileskellä.

## How to:
Koodinäytteet ja esimerkkitulosteet

```Lua
print("Hei maailma!")  -- Tavallinen tervehdys
```

Tuloste: Hei maailma!

```Lua
local x = 10
print("x:n arvo on:", x)  -- Muuttujan arvon tarkastus
```

Tuloste: x:n arvo on: 10

```Lua
-- Funktion suorituksen seuranta
local function kerro(a, b)
    print("Kerro funktio: a =", a, "ja b =", b)
    return a * b
end
print(kerro(3, 4))
```

Tuloste:
Kerro funktio: a = 3 ja b = 4
12

## Deep Dive
Syvä sukellus

Alkujaan, koodin suorituksen seuraaminen oli hyvin yksinkertaista: tulostuslauseita siroteltiin pitkin koodia siellä täällä. Vähitellen kehitettiin kehittyneempiä työkaluja, kuten interaktiivisia debuggereita, jotka antoivat ohjelmoijille tarkemmat välineet suorituksen seurantaan ja muuttujien tarkasteluun.

Lua-tulostus on yksinkertaisimmillaan vain `print`-funktion käyttöä, mutta vaihtoehtoja löytyy. `io.write` on toinen tapa, joka tarjoaa hienovaraisemman kontrollin tulostuksen formaattiin - se ei lisää automaattisesti rivinvaihtoa loppuun.

```Lua
io.write("Tämä on ", "vierekkäin ", "ilman rivinvaihtoa.\n")
```

Tuloste: Tämä on vierekkäin ilman rivinvaihtoa.

`tostring`-funktio on hyödyllinen, kun tarvitsee muuttaa ei-merkkijonotyypin arvoja merkkijonoesitykseksi tulostusta varten.

## See Also
Lisätietoja

- Lua 5.4:n viralliset dokumentit: https://www.lua.org/manual/5.4/
- Lua-käyttäjien wiki, jossa on käytännön vinkkejä ja esimerkkejä: http://lua-users.org/wiki/
- Stack Overflow, keskusteluja aiheesta Lua ja debuggaus: https://stackoverflow.com/questions/tagged/lua+debugging
