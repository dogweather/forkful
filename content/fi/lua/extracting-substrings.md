---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Alastringin erottaminen on prosessi, jossa otamme osan tekstistä - eli stringistä - erilliseksi stringiksi. Tätä tehdään, kun haluamme käsitellä vain kyseistä osaa stringistä tai kun haluamme verrata tiettyjä stringin osia muihin arvoihin.

## Kuinka:
Lua:n versiossa 5.3, stringin osan erottaminen on yksinkertaista. Lua:ssa on valmiina funktio `string.sub()`. Laitetaanpa koodiin esimerkkitapaus.

```Lua
teksti = "Ohjelmointi on hauskaa"
print(string.sub(teksti, 1, 12)) -- tulostaa: Ohjelmointi
print(string.sub(teksti, 14)) -- tulostaa: hauskaa
```

Tässä koodissa `string.sub()` -funktio ottaa kolme argumenttia: alkuperäinen stringi, aloituspositio ja lopetuspositio. Jos lopetuspositiota ei ole määritelty, funktio palauttaa alistringin aloituspositiosta alkaen loppuun asti.

## Syväsukellus
Lua:ssa substringin erottaminen on tehty helppoa `string.sub` -funktiolla, mutta se ei tarkoita ettei muita vaihtoehtoja olisi. Voit esimerkiksi käyttää `string.match` -funktiota, joka sallii monimutkaisemmat hakuoperaatiot regular expressionsin avulla. 

Aikaisemmin, ennen `string.sub` -funktion olemassaoloa, ohjelmoijat käyttivät looppeja ja muita ohjelmanrakenteita saavuttaakseen sama tavoitteen. 

## Katso Myös
Lisätietoa ja syvempää ymmärrystä koodin eri osista:
- Lua ohjekirja – String Library: https://www.lua.org/manual/5.3/manual.html#6.4
- String-viite – Lua-ohjelmointi: https://www.tutorialspoint.com/lua/lua_strings.htm
- Lua:ssa regular expressions: https://www.lua.org/pil/20.2.html