---
date: 2024-01-20 17:38:54.255310-07:00
description: "How to: \"Sukellus syv\xE4lle\" Lua:ssa merkkijonon muuttaminen pieniksi\
  \ kirjaimiksi k\xE4ytt\xE4\xE4 `string.lower`-funktiota. Historiallisesti, t\xE4\
  m\xE4 on per\xE4isin C:n\u2026"
lastmod: '2024-04-05T22:51:10.837198-06:00'
model: gpt-4-1106-preview
summary: "\"Sukellus syv\xE4lle\" Lua:ssa merkkijonon muuttaminen pieniksi kirjaimiksi\
  \ k\xE4ytt\xE4\xE4 `string.lower`-funktiota."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to:
"Kuinka tehdään:"
```Lua
local teksti = "Hei Maailma!"
local pieniksi = teksti:lower()
print(pieniksi)  -- tulostaa "hei maailma!"
```

## Deep Dive
"Sukellus syvälle"
Lua:ssa merkkijonon muuttaminen pieniksi kirjaimiksi käyttää `string.lower`-funktiota. Historiallisesti, tämä on peräisin C:n kirjastofunktioista. Vaihtoehtoina, voit käyttää myös UTF-8 kirjastoja, jos tarvitset laajempaa tukea erikoismerkeille.

Yleensä, `string.lower` on riittävä, mutta jos käsittelet monikielisiä tekstejä, saatat tarvita erikoiskirjastoja kuten `luautf8`. Nämä käsittelisivät esimerkiksi ääkkösiä paremmin.

```Lua
local utf8 = require('luautf8')
local tervehdys = "Moi Kaikki!"
local pieniksiUtf8 = utf8.lower(tervehdys)
print(pieniksiUtf8)  -- tulostaa "moi kaikki!"
```

## See Also
"Näitä myös kannattaa katsoa"
- Lua 5.4 referenssi: https://www.lua.org/manual/5.4/
- Lua käsikirja (Englanniksi): https://www.lua.org/pil/contents.html
- UTF-8 kirjaston dokumentaatio: https://github.com/starwing/luautf8
