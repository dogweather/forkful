---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tiedostosta tietojen lukemista merkkijonoina. Koodaajat tekevät sen hakiessaan tietoja tiedostoista.

## Miten tehdään:

Luin tiedoston lua-koodauksella näin:

```Lua
local tiedosto = io.open("testi.txt", "r") -- tiedoston avaus lukutilassa
if tiedosto then 
    for rivi in tiedosto:lines() do -- käydään tiedoston rivit läpi
        print(rivi) -- tulostetaan rivi
    end 
    tiedosto:close() -- tiedoston sulkeminen
else
    print("Ei voitu avata tiedostoa!") -- virheenkäsittely
end
```

Koodin ajo tulostaa tiedostossa olevat rivit.

## Syvempi sukellus

Lua perustuu alun perin 1993 julkaistuun Fast Light Toolkit -kirjastoon. Sen syntyyn vaikutti halu kevyempään ja nopeampaan ohjelmointityökaluun.

Vaihtoehtoisia keinoja tiedostojen lukemiseen on, kuten 'io.lines' sijaan käyttää 'io.read'. Erilaisia lukuoptioita on esimerkiksi '*line' (lue seuraava rivi), '*all' (lue kaikki) ja '*number' (lue seuraava numero).

Tiedoston lukeminen tapahtuu puskuroimalla, joka pitää sisällään datan lukemisen kerrallaan suuremmassa osassa kuin yksi tavu, mikä tekee prosessista nopeampi. 

## Katso myös:

[Lua Ohjelmoinnin Perusteet](https://ohjelmointi.net/lua)

[Lua Kielen Historia](https://fi.wikipedia.org/wiki/Lua_(ohjelmointikieli)) 

[Tietoa Tiedoston Lukemisesta](https://www.tutorialspoint.com/lua/lua_file_io.htm)