---
date: 2024-01-26 01:46:03.973591-07:00
description: "Koodin uudelleenj\xE4rjestely on taidetta, jossa olemassa olevaa koodia\
  \ muokataan parantamaan sen rakennetta, luettavuutta ja tehokkuutta muuttamatta\
  \ sen\u2026"
lastmod: '2024-03-13T22:44:56.705198-06:00'
model: gpt-4-0125-preview
summary: "Koodin uudelleenj\xE4rjestely on taidetta, jossa olemassa olevaa koodia\
  \ muokataan parantamaan sen rakennetta, luettavuutta ja tehokkuutta muuttamatta\
  \ sen ulkoista k\xE4ytt\xE4ytymist\xE4."
title: Koodin refaktorointi
weight: 19
---

## Miten:
Katsotaanpa yksinkertaista Luassa kirjoitettua funktiota ja uudelleenjärjestellään sitä. Aloitemme funktiolla, joka laskee numeroiden summan listassa mutta on kirjoitettu tehokkuutta ja selkeyttä paljoa ajattelematta:

```Lua
function sumList(numbers)
    local tulos = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                tulos = tulos + numbers[i]
            end
        end
    end
    palauta tulos
end

print(sumList({1, 2, 3, 4})) -- Tulostaa: 10
```

Uudelleenjärjestä tehokkaammaksi ja luettavammaksi versioksi:
```Lua
function sumListRefactored(numbers)
    local tulos = 0
    for _, arvo in ipairs(numbers) do
        tulos = tulos + arvo
    end
    palauta tulos
end

print(sumListRefactored({1, 2, 3, 4})) -- Tulostaa edelleen: 10
```

Uudelleenjärjestetty versio poistaa tarpeettoman sisemmän silmukan käyttäen `ipairs`-funktiota iteroidakseen listan läpi siististi.

## Syventävä katsaus
Historiallisesti koodin uudelleenjärjestely juontaa juurensa Smalltalk-ohjelmointiyhteisöstä 80-luvun lopulla ja Martin Fowlerin kirja 'Refactoring: Improving the Design of Existing Code' popularisoi sitä. Luassa koodin uudelleenjärjestelyssä on usein kyse monimutkaisten ehtolauseiden yksinkertaistamisesta, suurten funktioiden jakamisesta pienempiin osiin, ja taulukon käytön optimoinnista suorituskyvyn parantamiseksi.

Koodin uudelleenjärjestelyssä Luassa on omat varotoimensa; Luam kielelle ominainen dynaamisuus ja joustava tyypitys voivat tehdä tietyistä uudelleenjärjestelyistä, kuten muuttujien nimeämisen muuttamisesta tai funktioiden allekirjoitusten muuttamisesta, riskialttiimpia, jos niitä ei tehdä varovaisesti. Staattisen koodianalyysin työkalut (kuten `luacheck`) voivat vähentää tällaisia riskejä. Vaihtoehtoihin kuuluu testivetoinen kehitys (TDD), jossa koodia uudelleenjärjestellään jatkuvasti olennaisena osana kehitysprosessia, vastakohtana erilliselle uudelleenjärjestelyvaiheelle.

## Katso myös
- "Programming in Lua" Roberto Ierusalimschyltä parhaiden käytäntöjen ja esimerkkien saamiseksi.
- "Refactoring: Improving the Design of Existing Code" Martin Fowlerilta, periaatteet, jotka pätevät eri kielissä.
- LuaRocks-hakemisto (https://luarocks.org/) työkaluille ja moduuleille, jotka on suunnattu Luam koodin ylläpitoon ja uudelleenjärjestelyyn.
