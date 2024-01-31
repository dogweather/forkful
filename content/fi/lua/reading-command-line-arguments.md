---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:56:36.455037-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä & Miksi?  
Komentoriviparametrien lukeminen mahdollistaa argumenttien vastaanottamisen suoraan ohjelmallesi. Käytämme sitä, jotta käyttäjät voivat syöttää dataa ja määrittää ohjelman käyttäytymisen jo käynnistysvaiheessa.

## How to:
Miten tehdään:

```Lua
-- tallenna.lua
-- Otetaan komentoriviparametrit talteen
-- Käytetään globaalia 'arg' taulukkoa

-- 'arg' sisältää komentoriviparametrit (arguments)
if #arg < 1 then
  print("Anna vähintään yksi argumentti.")
else
  for i, param in ipairs(arg) do
    print("Argumentti " .. i .. ": " .. param)
  end
end

-- Käytä: lua tallenna.lua moikka maailma
```

Käytä seuraavaa komentoa komentorivillä:
```
lua tallenna.lua hei terve
```

Saadaksesi seuraavaa tulostetta:
```
Argumentti 1: hei
Argumentti 2: terve
```

## Deep Dive
Syväsukellus:

Lua otti käyttöön globaalin `arg` taulukon komentoriviparametrien käsittelyyn. Ennen Lua 5.0-versiota `arg` oli osa `*arg` globaalia muuttujaa tai saatettiin lukea suoraan funktiolle annetuista argumenteista. Taulukon indeksi 0 sisältää skriptin nimen, ja itse argumentit alkavat indeksistä 1.

Vaihtoehtoisesti voi käyttää standardikirjastoja, kuten `lapp` tai `penlight`, jotka tarjoavat monipuolisempaa käsittelyä, kuten optioanalysointia ja oletusarvoja.

Komentoriviparametrien lukemiseen liittyy joskus turvallisuusnäkökohtia, kuten syötteiden validointi ja escapen käsitteleminen, jotta vältetään injektio- ja suorituskykyongelmia.

## See Also
Katso myös:

- Lua Users Wiki on command-line arguments: http://lua-users.org/wiki/CommandLineArguments
- Argparse Lua module for handling command-line options: https://github.com/mpeterv/argparse
- "Programming in Lua" by Roberto Ierusalimschy: https://www.lua.org/pil/25.3.html
