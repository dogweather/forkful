---
title:                "Tekstitiedoston lukeminen."
html_title:           "Lua: Tekstitiedoston lukeminen."
simple_title:         "Tekstitiedoston lukeminen."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin tiedostojen lukeminen tarkoittaa tietyn tekstin sisällön lataamista tiedostosta jollekin tietokoneen ohjelmalle. Ohjelmoijat tekevät tätä esimerkiksi käyttäjän syöttämän tekstin lähettämisen yhteydessä, jotta ohjelma voi käsitellä ja näyttää kyseisen tekstin.

## Miten:

Esimerkki tiedoston lukemisesta Lua-koodilla:

```Lua
local tiedosto = io.open("tiedostonimi.txt", "r") -- avataan tiedosto lukutilassa
local sisältö = tiedosto:read("*a") -- luetaan tiedoston sisältö muuttujaan
tiedosto:close() -- suljetaan tiedosto

print(sisältö) -- tulostetaan teksti konsolille
```

Tässä koodissa tiedosto avataan tiedoston avulla ja sen sisältö luetaan muuttujaan. Lopuksi tiedosto suljetaan ja tulostetaan sisältö konsolille.

Tämä koodi toimii, jos tiedostonimi on muuttujassa `tiedostonimi.txt`. Voit myös antaa tai muuttaa tiedostonimeä halutessasi.

## Syvempi sukellus:

Tiedostojen lukeminen on tärkeä osa ohjelmointia, koska se mahdollistaa tekstin lähettämisen ja käsittelyn. Historiallisesti, tekstitiedostoja käytettiin pääasiassa tallentamaan ja jakamaan tietoa ennen kuin graafiset käyttöliittymät ja tietokannat olivat yleistyneet.

Muita tapoja lukea tiedostoja on esimerkiksi `io.lines()` ja `io.read()`, mutta `io.open()` tarjoaa eniten hallintaa tiedoston avaamiselle ja luvulle.

## Katso myös:

[Lua Reference Manual - io library](https://www.lua.org/manual/5.4/manual.html#6.8)