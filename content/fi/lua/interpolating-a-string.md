---
title:                "Merkkijonon interpolointi"
html_title:           "Lua: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Stringin interpolaatio on tapa yhdistää muuttujia ja tekstejä yhdeksi merkkijonoksi. Tämä on hyödyllistä silloin kun halutaan luoda dynaamisia viestejä, jotka vaihtelevat ympäristön tai käyttäjän mukaan. Tämä on yleinen käytäntö ohjelmoijien keskuudessa, joka nopeuttaa koodin kirjoittamista ja parantaa lopputulosta.

## Miten?
Esimerkki osoittaa, kuinka voit käyttää stringin interpolaatiota Lua-kielellä:

```Lua
-- Luo muuttujat
local nimi = "Matti"
local ikä = 30
-- Käytä stringin interpolaatiota
print("Tervetuloa, " .. nimi .. ". Olet " .. ikä .. " vuotta vanha.") 
```

Tulostus:

`Tervetuloa, Matti. Olet 30 vuotta vanha.`

Voit myös käyttää stringin interpolaatiota merkkijonon sisällä " $" -merkkien avulla:

```Lua
-- Luo muuttujat
local nimi = "Matti"
local ikä = 30
-- Käytä stringin interpolaatiota
print("Tervetuloa, ${nimi}. Olet ${ikä} vuotta vanha.") 
```

Tulostus:

`Tervetuloa, Matti. Olet 30 vuotta vanha.`

Huomaa, että täytyy käyttää erillistä kirjastoa kuten "string.format" lisätäksesi muotoilua merkkijonon sisällä.

## Syväsukellus
Stringin interpolaatiolla on ollut pitkä historia ohjelmoinnissa ja se löytyy monista kielistä, kuten C ja Python. Se on usein parempi vaihtoehto kuin käyttää yksittäisiä merkkijonoja muuttujien kanssa, sillä se on yleensä helpompaa lukea ja ylläpitää.

Voit myös käyttää merkkijonojen yhdistämisen sijaan Lua-kielen "string.format" -toimintoa, joka tarjoaa enemmän muotoilumahdollisuuksia. Voit lukea lisää tästä Lua-käsikirjasta: https://www.lua.org/manual/5.3/manual.html#pdf-string.format

Stringin interpolaation toteutus Lua-kielessä perustuu Lua-metodoihin, jotka tarjoavat joustavan tavan yhdistää merkkijonoja ja muuttujia.

## Katso myös
- Lua-käsikirja: https://www.lua.org/manual/5.3/
- Ohjelmointiopas: https://www.tutorialspoint.com/lua/lua_string_interpolation.htm
- GitHub esimerkkejä: https://github.com/search?q=lua+string+interpolation