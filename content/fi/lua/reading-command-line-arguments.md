---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Käskyriviparametrien Lukemista Lua-Kielellä

## Mikä & Miksi?

Käskyriviparametrien lukeminen viittaa prosessiin, jossa ohjelma vastaanottaa ja käsittelee käyttäjän syöttämät tiedot käynnistysvaiheessa. Koodarit tekevät tämän jotta he voivat ohjeistaa ohjelmaa tekemään haluttunsa perusteella käyttäjän tarpeita.

## Miten toimii:

```Lua
-- Hae argumentit 
local args = {...}

-- Tulosta argumentit
for i, arg in ipairs(args) do
    print("Argumentti " .. i .. ": " .. arg)
end
```

Kun ajetaan koodi yläpuolella `lua script.lua Hello, Lua!`, ohjelma tulostaa seuraavan:

```Lua
Argumentti 1: Hello,
Argumentti 2: Lua!
```

## Syvällinen Sukellus

Lua-kielessä käskyriviparametrit esitellään taulukossa, johon pääsee käsiksi käyttämällä kolmea pistettä (`...`). Tämä tapa otettiin käyttöön Lua-julkaisun 5.1 myötä vuonna 2006; aiemmissa versioissa käytettiin globaalia taulukkoa nimeltä arg.

Vaihtoehtona, voi käyttää io-moduulia syöttö- ja lähtötoimintojen, myös syötteiden, hankkimiseen käyttäjältä. Tämä menetelmä on joustavampi, mutta saattaa olla monimutkaisempi uusille Lua-koodareille.

Lisätiedot, kuten matalamman tason toteutuksen yksityiskohdat, yleensä riippuvat käytetyn Lua-toteutuksen syvemmistä yksityiskohdista.

## Katso Myös

- Lua-Kieliopas: [Programming in Lua](https://www.lua.org/pil/contents.html)
- Lisää Lua-ohjelmointiesimerkkejä: [Learn X in Y minutes](https://learnxinyminutes.com/docs/lua/)

Niin siinä. Toivottavasti tämä artikkeli antoi selkeyttä siitä, miten Lua käsittelee käskyriviparametreja!