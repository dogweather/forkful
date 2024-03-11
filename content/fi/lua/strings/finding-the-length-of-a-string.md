---
date: 2024-01-20 17:47:39.098598-07:00
description: "Mittauksella tarkoitetaan merkkijonon pituuden selvitt\xE4mist\xE4.\
  \ Ohjelmoijat tarvitsevat t\xE4t\xE4 toimintoa, jotta voivat k\xE4sitell\xE4 tekstidataa\
  \ tehokkaasti ja\u2026"
lastmod: '2024-03-11T00:14:30.641506-06:00'
model: gpt-4-1106-preview
summary: "Mittauksella tarkoitetaan merkkijonon pituuden selvitt\xE4mist\xE4. Ohjelmoijat\
  \ tarvitsevat t\xE4t\xE4 toimintoa, jotta voivat k\xE4sitell\xE4 tekstidataa tehokkaasti\
  \ ja\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## What & Why?
Mittauksella tarkoitetaan merkkijonon pituuden selvittämistä. Ohjelmoijat tarvitsevat tätä toimintoa, jotta voivat käsitellä tekstidataa tehokkaasti ja tehdä päätöksiä ohjelmissaan.

## How to:
Merkkijonon pituuden saat näin:

```Lua
local teksti = "Hei maailma!"
print(#teksti)  -- Tulostaa merkkijonon pituuden
```

Tulos:

```
12
```

## Deep Dive
Lua käyttää `#` operaattoria merkkijonon pituuden hakuun. Historiallisesti, tämä valinta on tehty sen yksinkertaisuuden ja tehokkuuden vuoksi. Vaihtoehtoisesti, voit käyttää `string.len`-funktiota, joka on equivaläntti `#` operaattorille.

```Lua
print(string.len(teksti))
```

Tämä toimii samalla tavalla. Merkkijono tallennetaan Lua:ssa 'byte array' muodossa, ja `#` operaattori laskee täten arrayn koon. Tämä tarkoittaa myös sitä, että Unicode-merkit, kuten ääkköset, voivat vaikuttaa laskettuun pituuteen, sillä ne saattavat olla enemmän kuin yhden byten pituisia.

## See Also
- Lua 5.4 referenssi: https://www.lua.org/manual/5.4/manual.html#3.4.7
- 'Programming in Lua' kirjan alkeet: https://www.lua.org/pil/11.5.html
- Lua string-funktioiden lista: https://www.lua.org/manual/5.4/manual.html#6.4
