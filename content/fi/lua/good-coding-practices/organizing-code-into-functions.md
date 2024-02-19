---
aliases:
- /fi/lua/organizing-code-into-functions/
date: 2024-01-26 01:11:34.374469-07:00
description: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa k\xE4sikirjoituksesi\
  \ jakamista pieniin palasiin\u2014ajattele funktionaalisia LEGO-palikoita. Teemme\
  \ n\xE4in selkeyden,\u2026"
lastmod: 2024-02-18 23:09:07.762865
model: gpt-4-1106-preview
summary: "Koodin j\xE4rjest\xE4minen funktioihin tarkoittaa k\xE4sikirjoituksesi jakamista\
  \ pieniin palasiin\u2014ajattele funktionaalisia LEGO-palikoita. Teemme n\xE4in\
  \ selkeyden,\u2026"
title: "Koodin j\xE4rjest\xE4minen funktioihin"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa käsikirjoituksesi jakamista pieniin palasiin—ajattele funktionaalisia LEGO-palikoita. Teemme näin selkeyden, uudelleenkäytettävyyden ja järkevyyden vuoksi. Se tekee koodistamme siistiä, luettavaa ja ylläpidettävää.

## Kuinka:
```Lua
-- Määritellään yksinkertainen tervehdysfunktio
function greet(name)
    return "Hei, " .. name .. "!"
end

-- Käytetään funktiota
print(greet("Lua-ohjelmoija")) -- Esimerkkitulostus: Hei, Lua-ohjelmoija!
```

Funktiot muuttuvat monimutkaisemmiksi ja käsittelevät erilaisia tehtäviä:
```Lua
-- Funktio suorakulmion alueen laskemiseksi
function calculateArea(width, height)
    return width * height
end

-- Kutsutaan funktiota ja tulostetaan tulos
local area = calculateArea(5, 4)
print(area)  -- Esimerkkitulostus: 20
```

## Syväsukellus
Lua, syntymästään 90-luvulla lähtien, on kannustanut modulaariseen suunnitteluun. Koodin järjestäminen funktioihin ei ole ainutlaatuista Luassa—se on ollut käytäntönä ohjelmointikielten, kuten Fortranin ja Lispin, sarastuksesta asti. Vaihtoehdot, kuten sisäinen koodi tai saman koodin kopiointi ja liittäminen yli, eivät ole vain paheksuttavia; ne ovat potentiaalisia virhepesäkkeitä.

Luassa funktiot ovat ensiluokkaisia kansalaisia, mikä tarkoittaa, että niitä voidaan tallentaa muuttujiin, välittää argumentteina ja palauttaa toisista funktioista. Ne ovat monipuolisia. Luamoodin yksisäikeisyyden vuoksi on tärkeää pitää funktiot tiukkoina ja tehokkaina suorituskyvyn säilyttämiseksi. Funktiot voivat olla paikallisia (skoopattuja) tai globaaleja, ja ymmärrys siitä, milloin kumpaakin kannattaa käyttää, voi tehdä tai rikkoa skriptisi tehokkuuden.

## Katso myös
- Virallinen Luamoodin dokumentaatio funktioista: https://www.lua.org/pil/6.html
- Käytännön esimerkkejä funktioiden käytöstä Luamoodissa: https://lua-users.org/wiki/SampleCode
- Siistin koodin käytännöt Luamoodissa: https://github.com/Olivine-Labs/lua-style-guide
