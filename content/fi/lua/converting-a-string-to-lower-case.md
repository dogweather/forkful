---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- fi/lua/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:54.255310-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä ja miksi?"
Muuttaa merkkijonon isot kirjaimet pieniksi. Tämä on hyödyllistä kun halutaan vertailla tekstejä välittämättä kirjainkoosta, kuten käyttäjän syötteiden standardisoinnissa.

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
