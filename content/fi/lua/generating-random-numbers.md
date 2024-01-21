---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:44.309150-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Arpajaisnumeron tuotto tarkoittaa satunnaisten numeroiden generointia. Ohjelmoijat käyttävät sitä pelilogiikan, tilastollisen analyysin ja turvallisuuden parantamiseen.

## How to: (Kuinka tehdä:)
```Lua
-- Alustetaan generaattori
math.randomseed(os.time())

-- Arvotaan kokonaisluku väliltä 1 - 10
local numero = math.random(1, 10)
print("Arvottu numero on: " .. numero)

-- Arvotaan liukuluku väliltä 0 - 1
local liukuluku = math.random()
print("Arvottu liukuluku on: " .. liukuluku)
```
Output:
```
Arvottu numero on: 7
Arvottu liukuluku on: 0.43721804285056
```

## Deep Dive (Sukellus syvemmälle):
Random-luvut olivat alun perin manuaalisten menetelmien tulos, kuten noppien heitto. "Pseudosatunnainen" luonti alkoi tietokoneiden myötä, mikä tarkoittaa deterministisiä algoritmeja. Lua-kielessä, `math.randomseed` antaa siemenarvon `math.random`-funktiolle, joka vaikuttaa tuloksiin.

Vaihtoehtoja Lua-arvontaan sisältävät kolmannen osapuolen kirjastot, jotka tarjoavat paremman randomisoinnin tai spesifisiä tarpeita varten kuten krypto-graafinen satunnaisuus.

Yksi tärkeä seikka random-lukujen generoinnissa on siemenarvon ("seed value") asettaminen, mikä varmistaa, että lukusarjat eivät ole samat joka käynnistys. `os.time()` tarjoaa hyvän, aikaan perustuvan siemenarvon.

## See Also (Katso myös):
- Lua-users.org wiki (eng.): "RandomNumbers" - [http://lua-users.org/wiki/RandomNumbers](http://lua-users.org/wiki/RandomNumbers)
- Lua 5.4 Reference Manual (eng.): "math Library" - [https://www.lua.org/manual/5.4/manual.html#6.7](https://www.lua.org/manual/5.4/manual.html#6.7)
- "Numerical Recipes in C" -kirja tarjoaa yksityiskohtaisen näkymän satunnaislukujen generoinnin algoritmeihin (englanniksi).