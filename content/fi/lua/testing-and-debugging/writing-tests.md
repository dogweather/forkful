---
title:                "Testien kirjoittaminen"
aliases:
- fi/lua/writing-tests.md
date:                  2024-02-03T19:32:06.261443-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Ohjelmoinnissa testien kirjoittaminen tarkoittaa pienten, erillisten koodinpätkien luomista automaattisesti varmistamaan, että sovelluksesi eri osat toimivat odotetusti. Lua-ohjelmoijille testaus takaa luotettavuuden ja auttaa ylläpitämään koodin laatua, nopeuttaa vianetsintäprosessia ja tekee koodikannan muokkauksista turvallisempia.

## Kuinka:

Lua on kevyt, mutta tehokas skriptauskieli, eikä sisällä sisäänrakennettua testauskehystä. Kolmannen osapuolen kirjastot, kuten Busted ja LuaUnit, tekevät testauksesta suhteellisen suoraviivaista. Tutustumme esimerkkeihin käyttäen molempia.

### Käyttäen Bustedia

Busted on suosittu Lua-testauskehys, joka tarjoaa joustavan tavan kirjoittaa testejä. Asenna Busted ensin LuaRocksin (Luas paketinhallinta) kautta komennolla `luarocks install busted`. Asennuksen jälkeen voit kirjoittaa testejäsi. Tässä on yksinkertainen testi funktiolle `add`, joka summaa kaksi lukua:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add-funktio", function()
  it("pitäisi laskea kaksi numeroa oikein", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Suorittaaksesi testit, suorita komento `busted` terminaalissasi. Esimerkki läpäisseen testin tulosteesta näyttäisi tältä:

```
●
1 onnistuminen / 0 epäonnistumista / 0 virhettä / 0 odottamassa : 0.002 sekuntia
```

### Käyttäen LuaUnitia

LuaUnit on toinen testauskehys, joka noudattaa xUnit-konventioita ja on helppo ottaa käyttöön. Asenna LuaUnit LuaRocksilla käyttäen komentoa `luarocks install luaunit`. Tässä on miten voisit kirjoittaa samanlaisen testin kuin yllä LuaUnitin kanssa:

```lua
-- add.lua pysyy samana

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Suorittamalla tämän skriptin suoraan Lualla (`lua test_add.lua`) tulisi tuloste jotain tällaista:

```
.
Suoritettiin 1 testiä 0.001 sekunnissa, 1 onnistuminen, 0 epäonnistumista
```

Sekä Busted että LuaUnit tarjoavat kattavia ominaisuuksia käsittelemään erilaisia testausskenaarioita, mukaan lukien teeskentelyn, vakoilun ja asynkronisen testauksen. Niiden välillä valinta riippuu projektisi erityistarpeista ja henkilökohtaisesta mieltymyksestäsi syntaksin ja toiminnallisuuden suhteen.
