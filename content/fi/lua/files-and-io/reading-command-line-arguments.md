---
date: 2024-01-20 17:56:36.455037-07:00
description: "How to: Syv\xE4sukellus: Lua otti k\xE4ytt\xF6\xF6n globaalin `arg`\
  \ taulukon komentoriviparametrien k\xE4sittelyyn. Ennen Lua 5.0-versiota `arg` oli\
  \ osa `*arg` globaalia\u2026"
lastmod: '2024-04-05T22:51:10.865372-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4sukellus: Lua otti k\xE4ytt\xF6\xF6n globaalin `arg` taulukon komentoriviparametrien\
  \ k\xE4sittelyyn. Ennen Lua 5.0-versiota `arg` oli osa `*arg` globaalia muuttujaa\
  \ tai saatettiin lukea suoraan funktiolle annetuista argumenteista. Taulukon indeksi\
  \ 0 sis\xE4lt\xE4\xE4 skriptin nimen, ja itse argumentit alkavat indeksist\xE4 1.\
  \ Vaihtoehtoisesti voi k\xE4ytt\xE4\xE4 standardikirjastoja, kuten `lapp` tai `penlight`,\
  \ jotka tarjoavat monipuolisempaa k\xE4sittely\xE4, kuten optioanalysointia ja oletusarvoja.\
  \ Komentoriviparametrien lukemiseen liittyy joskus turvallisuusn\xE4k\xF6kohtia,\
  \ kuten sy\xF6tteiden validointi ja escapen k\xE4sitteleminen, jotta v\xE4ltet\xE4\
  \xE4n injektio- ja suorituskykyongelmia."
title: Komennoriviparametrien lukeminen
weight: 23
---

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
