---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:43:17.953657-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kompleksiluvut laajentavat yksiulotteisen lukusuoran ideaa kaksiulotteiseen tasoon sisällyttämällä pystysuoran imaginääriakselin. Ohjelmoijat työskentelevät niiden parissa aloilla kuten signaalinkäsittely, fluididynamiikka ja sähkötekniikka, joissa ne ovat olennaisia esittämään oskillaatioita ja muita ilmiöitä.

## Miten:
Luassa voit esittää kompleksilukuja taulukoiden avulla. Perustoiminnot sisältävät taulukoiden lisäämisen, vähentämisen, kertomisen ja jakamisen. Näin se tapahtuu:

```lua
-- Määritä kaksi kompleksilukua taulukoina
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Funktio kahden kompleksiluvun lisäämiseen
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Esimerkkituloste
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Syväsukellus
Kompleksiluvut ovat olleet olemassa 1500-luvulta lähtien, auttaen ratkaisemaan yhtälöitä, joita ei voitu selvittää pelkillä reaaliluvuilla. Luassa itsessään ei ole sisäänrakennettua kompleksiluvun tyyppiä. Tämä ei kuitenkaan ole suuri ongelma—voit luoda omia kompleksilukujen käsittelyjä käyttämällä taulukoita ja funktioita, kuten yllä on esitetty. Tai, jos tarpeesi ovat syvemmät, voit hankkia kirjaston kuten LuaComplex. Se on hyvä valinta, koska se on erityisesti Lualle rakennettu ja ottaa manuaalityön pois harteiltasi. Tällaiset kirjastot myös usein optimoivat operaatioita taustalla, joten ne ovat nopeampia kuin omien ratkaisujen kehittäminen.

## Katso Myös
Lisää yksityiskohtaisia esimerkkejä ja edistyneitä operaatioita varten, tutustu näihin:

- LuaComplex-kirjasto: https://github.com/davidm/lua-complex
- "Programming in Lua" -kirja, mukautettujen tietotyyppien luomisesta: https://www.lua.org/pil/11.1.html
- Wikipedia kompleksilukujen käytöstä eri aloilla: https://en.wikipedia.org/wiki/Complex_number#Applications
