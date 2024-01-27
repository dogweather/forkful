---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Kirjoittaminen standardivirheeseen (stderr) on tapa tulostaa virheviestit ohjelmastasi. Ohjelmoijat käyttävät sitä erotellakseen normaalin tulosteen (stdout) ja virhetiedot, mikä auttaa virheiden diagnosoinnissa.

## How to: (Kuinka tehdä:)
```Lua
-- Kirjoitetaan virhe stderr:iin
io.stderr:write("Tapahtui virhe!\n")

-- Jos haluat käyttää print-funktiota, ohjaa se stderr:iin
local old_print = print
print = function(...)
    old_print(...)
    io.stderr:write("Virhe: ", ...)
    io.stderr:write("\n")
end

-- Käytetään muokattua print-funktiota virheen näyttämiseen
print("Tämä on virheviesti")
```

Tuloste (esimerkki):

```
Tämä on virheviesti
```

## Deep Dive (Syvä sukellus)
Alusta asti, stderr on ollut yksi kolmesta Unix-standardivirrasta, muiden ollessa stdin (standardisyöte) ja stdout (standardituloste). Kun tulostat stderr:iin, viestisi menevät yleensä komentoriville tai virhelokiin, sen sijaan että ne sekoittuisivat normaaliin tulosteeseen. Lua käyttää `io`-kirjastoa tähän, ja `io.stderr:write()` on suorin tapa kirjoittaa virhetulostus. Lua:ssa voit myös ohjata `print`-funktiota kirjoittamaan stderr:iin, kuten yllä olevassa esimerkissä.

## See Also (Katso myös)
- Lua 5.4 Reference Manual `io` library: https://www.lua.org/manual/5.4/manual.html#6.8
- Unix Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
- Effective Lua Debugging Techniques: https://www.lua.org/pil/21.3.html
