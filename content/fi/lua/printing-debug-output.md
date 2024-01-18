---
title:                "Tulostetaan debuggaus-tulosteita"
html_title:           "Lua: Tulostetaan debuggaus-tulosteita"
simple_title:         "Tulostetaan debuggaus-tulosteita"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Tulostaminen debug-tulosteena on yksinkertainen tapa tarkistaa koodin suorittamisen vaiheita ohjelman aikana. Ohjelmoijat käyttävät sitä vianmäärityksen ja koodin toiminnan ymmärtämisen apuvälineenä.

# Miten:
Esimerkki koodin tulostamisesta debug-tulosteena:

```Lua
local luku = 10
print("Debug-tulostus: Luku on", luku)
```

Esimerkin tulostus:

```
Debug-tulostus: Luku on 10
```

# Syväsukellus:
## Historiallinen tausta:
Debug-tulostamisella on pitkä historia ohjelmoinnissa. Ennen modernimpia työkaluja, kuten debuggereita, ohjelmoijat joutuivat turvautumaan tulostamaan koodin suoritusvaiheita ymmärtääkseen, missä kohtaa koodi ei toimi odotetusti.

## Vaihtoehtoiset metodit:
Debug-tulostamisen lisäksi ohjelmoijat voivat käyttää muita vianmääritystyökaluja, kuten debuggereita ja testausohjelmia, joista voi olla hyötyä monimutkaisemmissa tapauksissa.

## Toteutus:
Debug-tulostamisessa käytetään yleensä "print" -funktiota, joka tulostaa halutut tiedot ohjelman konsoliin tai muuhun määritettyyn paikkaan. Ohjelmoija asettaa tulostettavan tiedon haluamaansa kohtaan koodia ja suorittaessaan ohjelman, hän näkee tulosteen ja voi sen avulla tarkistaa koodin suoritusvaiheita.

# Katso myös:
- [Lua:n viralliset verkkosivut](https://www.lua.org/)
- [Lua-kieliopas](https://www.tutorialspoint.com/lua/index.htm)
- [Lua Debugger](https://studio.zerobrane.com/)