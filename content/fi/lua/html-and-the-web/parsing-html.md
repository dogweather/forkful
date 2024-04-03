---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:46.725841-07:00
description: "HTML:n j\xE4sennys sis\xE4lt\xE4\xE4 tietojen ja datan poimimisen HTML-dokumenteista,\
  \ mik\xE4 on ratkaisevan t\xE4rke\xE4\xE4 verkkosivujen kaapauksessa, datan analysoinnissa\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.695199-06:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sennys sis\xE4lt\xE4\xE4 tietojen ja datan poimimisen HTML-dokumenteista,\
  \ mik\xE4 on ratkaisevan t\xE4rke\xE4\xE4 verkkosivujen kaapauksessa, datan analysoinnissa\
  \ ja automaatioteht\xE4viss\xE4."
title: "HTML:n j\xE4sennys"
weight: 43
---

## Kuinka:
Lua ei sisällä sisäänrakennettua kirjastoa HTML:n jäsennykseen, mutta voit hyödyntää kolmannen osapuolen kirjastoja kuten `LuaHTML` tai käyttää `libxml2`-sidoksia `LuaXML`-kirjaston kautta. Suosittu lähestymistapa on käyttää `lua-gumbo`-kirjastoa HTML:n jäsennykseen, joka tarjoaa suoraviivaisen, HTML5-standardin mukaisen jäsennyskyvyn.

### lua-gummon asentaminen:
Aluksi varmista, että `lua-gumbo` on asennettu. Sen voi yleensä asentaa luarocksilla:

```sh
luarocks install lua-gumbo
```

### Perusjäsennys lua-gumbolla:
Tässä on kuinka voit jäsentää yksinkertaisen HTML-katkelman ja poimia siitä tietoja käyttäen `lua-gumboa`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hei, maailma!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Tulostus: Hei, maailma!
```

### Edistynyt esimerkki - Linkkien poimiminen:
Poimiaksesi `href`-attribuutit kaikista ankkuritagieista (`<a>`-elementit) HTML-dokumentissa:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Esimerkkisivu</title></head>
<body>
  <a href="http://example.com/1">Linkki 1</a>
  <a href="http://example.com/2">Linkki 2</a>
  <a href="http://example.com/3">Linkki 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Varmista, että se on Element ja sillä on attribuutteja
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Esimerkkitulostus:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Tämä koodinpätkä iteroi läpi kaikki dokumentin linkit ja tulostaa niiden `href`-attribuutit. `lua-gumbo`-kirjaston kyky jäsentää ja ymmärtää HTML-dokumentin rakennetta yksinkertaistaa prosessia tietyillä tageilla tai attribuuteilla varustettujen elementtien poimimiseksi.
