---
date: 2024-01-20 17:47:39.098598-07:00
description: "How to: Merkkijonon pituuden saat n\xE4in."
lastmod: '2024-03-13T22:44:56.688966-06:00'
model: gpt-4-1106-preview
summary: "Merkkijonon pituuden saat n\xE4in."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

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
