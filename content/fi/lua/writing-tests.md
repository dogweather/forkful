---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Testaus tarkoittaa koodin laadun varmistamista automatisoiduilla testeillä. Ohjelmoijat testaavat löytääkseen ja korjatakseen virheitä, sekä varmistaakseen, että koodi tekee mitä pitääkin uusien ominaisuuksien tai muutosten jälkeen.

## How to: (Kuinka tehdään:)
```Lua
-- Yksinkertainen testifunktio
function summa(a, b)
    return a + b
end

-- Testataan 'summa' funktiota
local testitulos = summa(5, 3)
assert(testitulos == 8, "Odottamaton tulos: " .. testitulos)

print("Summafunctio toimii oikein!")
```

Esimerkin tulostus:

```
Summafunctio toimii oikein!
```

## Deep Dive (Syvä sukellus)
Lua ei sisällä sisäänrakennettua testaustyökalua, mutta yhteisö on luonut useita. Esimerkiksi *LuaUnit* on suosittu testikirjasto. Historiallisesti, ohjelmoijat kirjoittivat testejä manuaalisesti, mutta nyt automatisointi on standardi. Vaihtoehtoja on monia, kuten *Busted* tai *luassert* -kirjastot. Testit kannattaa kirjoittaa niin, että ne ovat eristettyjä, nopeita ja helposti ymmärrettäviä.

## See Also (Katso myös)
- LuaUnit: https://github.com/bluebird75/luaunit
- Busted, erillinen Lua testauskehyk (test framework): http://olivinelabs.com/busted/
- Luassert, vakuuttelu/assertion kirjasto: https://github.com/Olivine-Labs/luassert
