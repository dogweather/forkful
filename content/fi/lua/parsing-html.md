---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:32:38.710308-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
HTML-parsinta tarkoittaa HTML-koodin lukemista ja sen rakenteen käsittelemistä ohjelmallisesti. Sitä tarvitaan, kun halutaan esimerkiksi kaivaa tietoa verkkosivuilta tai muokata HTML-sisältöä automaattisesti.

## How to: (Kuinka tehdä:)
Lua ei oletuksena tue HTML-parsintaa, mutta käytännössä tarvitaan ulkoista kirjastoa, kuten luasoup. 

```Lua
local luasoup = require('luasoup')

-- Ladataan HTML-sisältö merkkijonona
local html_content = [[
<html>
    <head>
        <title>Esimerkkisivu</title>
    </head>
    <body>
        <h1>Tervetuloa!</h1>
        <p>HTML-parsinta on kivaa.</p>
    </body>
</html>
]]

-- Parsitaan HTML sisältö luasoup-kirjaston avulla
local soup = luasoup.parse(html_content)

-- Etsitään otsikko
local h1_tag = soup:select('h1')[1]
print(h1_tag:text())  -- Output: Tervetuloa!

-- Muutetaan kappaleen tekstiä
local p_tag = soup:select('p')[1]
p_tag:set_text('HTML-parsinta Lualla on sujuvaa.')
print(soup:select('p')[1]:text())  -- Output: HTML-parsinta Lualla on sujuvaa.
```

## Deep Dive (Sukellus syvemmälle)
Ennen luasoupia ja muita kirjastoja Lua-ohjelmoijat joutuivat turvautumaan säännöllisiin lausekkeisiin (regex), jotka ovat virhealttiita ja hankalia monimutkaisessa HTML:n käsittelyssä. Luasoup tarjoaa DOM-pohjaista lähestymistapaa, joka on luotettavampi ja luettavampi. Vaihtoehtoisesti voi käyttää LuaXML-kirjastoa XML:n ja HTML:n käsittelyyn, mutta se on vähemmän suosittu. Luasoup käyttää sisäisesti lxml parseria, joka perustuu libxml2-kirjastoon.

## See Also (Katso myös)
- [luaxml GitHub repository](https://github.com/LuaDist/luaxml)
