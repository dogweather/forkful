---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsennys tarkoittaa HTML-dokumentin rakenteen lukemista ja tulkitsemista. Ohjelmoijat tekevät tämän yleensä joko datan kaivamiseksi tai dokumentin muuntamiseksi johonkin toiseen formaattiin.

## Miten se tehdään:

```Lua
htmlparser = require("htmlparser")

local root = htmlparser.parse('<div>Hei, Suomi!</div>')

root("div"):each(function(_, div)
  print(div:getcontent())
end)
```
Suorittaessasi tämän koodin, tulostuu konsoliin: 'Hei, Suomi!'. Tämä on hyvin perustason esimerkki, jossa etsitään `div`-elementit ja tulostetaan niiden sisältö.

## Syvempi tieto:

HTML:n jäsennys on ollut olemassa lähes yhtä kauan kuin HTML itse, 1990-luvun alusta. Muita vaihtoehtoja jäsennykseen ovat esimerkiksi DOM-puun jäsennys tai Regular Expressions -menetelmät.

Lua:ssa HTML:n jäsentämistä varten on useita kirjastoja, esimerkiksi LuaHTML ja HTMLparser, joka on osa LuaSec-kirjastoa. Nuokin kirjastot käyttävät pääsääntöisesti lexikaalista analyysiä tagien erottamiseen ja DOM-puun luomiseen.

## Katso myös:

Lue lisää HTML:n jäsennyskirjastoista ja niiden käytöstä näiltä sivuilta:

- LuaHTML: https://luarocks.org/modules/craigb/luaxml
- LuaSec (johon HTMLparser kuuluu): https://github.com/brunoos/luasec
- DOM-puun käyttö HTML:n jäsennyksessä: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction
- Regular Expressions -menetelmä: https://www.regular-expressions.info/examples.html