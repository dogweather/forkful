---
date: 2024-01-20 17:46:22.798337-07:00
description: "How to: / Kuinka tehd\xE4: Lua-kielen string-handling-nojaa voimakkaasti\
  \ yhteenrakennettuihin funktioihin, kuten `sub` ja `match`. Historiallisesti Lua\u2026"
lastmod: '2024-04-05T22:51:10.838868-06:00'
model: gpt-4-1106-preview
summary: "/ Kuinka tehd\xE4: Lua-kielen string-handling-nojaa voimakkaasti yhteenrakennettuihin\
  \ funktioihin, kuten `sub` ja `match`. Historiallisesti Lua kasvoi embeddattujen\
  \ systeemien tarpeista: pieni footprintti, mutta tehokkaat string-ty\xF6kalut ovat\
  \ aina olleet korostettuja. Pattern-matching perustuu s\xE4\xE4nn\xF6llisiin lausekkeisiin\
  \ (regex), mutta on rajoitettumpi. `match` funktio sallii monimutkaisten patternien\
  \ tunnistamista stringeist\xE4 ilman ett\xE4 tarvitsee vet\xE4\xE4 sis\xE4\xE4n\
  \ kokonaisia regex-kirjastoja. Substring-funktioita k\xE4ytett\xE4ess\xE4 indeksit\
  \ alkavat ykk\xF6sest\xE4, ei nollasta, kuten monissa muissa kieliss\xE4. Negatiiviset\
  \ indeksit laskevat lopusta alkaen, mist\xE4 on usein hy\xF6ty\xE4."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: / Kuinka tehdä:
```Lua
local teksti = "tervetuloa_programmointiin!"
-- Alkuindeksi ja loppuindeksi
local substring = teksti:sub(1, 11)
print(substring) -- tulos: "tervetuloa_"

-- Negaatiiviset indeksit (lopusta alkaen)
local loppuosa = teksti:sub(-14)
print(loppuosa) -- tulos: "programmointiin!"

-- Pattern-matching leikkaus
local kayttajanimi = string.match("esimerkki@esimerkki.fi", "([^@]+)")
print(kayttajanimi) -- tulos: "esimerkki"
```

## Deep Dive / Syväsukellus:
Lua-kielen string-handling-nojaa voimakkaasti yhteenrakennettuihin funktioihin, kuten `sub` ja `match`. Historiallisesti Lua kasvoi embeddattujen systeemien tarpeista: pieni footprintti, mutta tehokkaat string-työkalut ovat aina olleet korostettuja.

Pattern-matching perustuu säännöllisiin lausekkeisiin (regex), mutta on rajoitettumpi. `match` funktio sallii monimutkaisten patternien tunnistamista stringeistä ilman että tarvitsee vetää sisään kokonaisia regex-kirjastoja.

Substring-funktioita käytettäessä indeksit alkavat ykkösestä, ei nollasta, kuten monissa muissa kielissä. Negatiiviset indeksit laskevat lopusta alkaen, mistä on usein hyötyä.

## See Also / Katso Myös:
- [Programming in Lua (Fourth Edition)](https://www.lua.org/pil/contents.html) – Luotettava kirja perusteista syvällisiin tekniikoihin.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) – Virallinen dokumentaatio ja käyttöopas.
