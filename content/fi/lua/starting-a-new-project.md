---
title:                "Uuden projektin aloittaminen"
html_title:           "Lua: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

##Mitä & Miksi?
Uuden projektin aloittaminen tarkoittaa uuden ohjelman tai sovelluksen luomista tyhjästä. Ohjelmoijat tekevät sitä esimerkiksi uuden idean toteuttamiseksi tai vanhan ohjelman parantamiseksi.

##Kuinka:
Esimerkki Lua-koodista, joka luo uuden projektin ja tulostaa "Hello World!"-tekstin:

```Lua
-- Luo uusi projektikansio ja tiedosto
os.execute("mkdir uusi_projekti")
os.execute("touch uusi_projekti/ohjelma.lua")

-- Avaa tiedosto ja kirjoita koodi
file = io.open("uusi_projekti/ohjelma.lua", "w")
file:write("print('Hello World!')")
file:close()

-- Suorita projekti
dofile("uusi_projekti/ohjelma.lua")
```

Tulostus:

```
Hello World!
```

##Syvällisemmin:
Uuden projektin aloittamisella voi olla historiallisia syitä, kuten uuden teknologian käyttöönotto tai vanhan ohjelman kehittämisen tarve. Lua on monipuolinen ohjelmointikieli, jota käytetään usein esimerkiksi pelimoottoreissa ja web-sovelluksissa. Muita vaihtoehtoja uuden projektin aloittamiseen voivat olla esimerkiksi Python tai JavaScript. Implementaation yksityiskohdat voivat vaihdella riippuen käytetystä alustasta ja ohjelmointiympäristöstä.

## Katso myös:
- Lua:n virallinen sivusto: https://www.lua.org/
- Lua-opetusohjelma: http://lua-users.org/wiki/TutorialDirectory
- Vertailu muihin ohjelmointikieliin: https://en.wikipedia.org/wiki/Comparison_of_programming_languages