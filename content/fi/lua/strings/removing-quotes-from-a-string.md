---
date: 2024-01-26 03:41:18.041675-07:00
description: "Kuinka: T\xE4ss\xE4 on miten heit\xE4t lainausmerkit nurkkaan Luassa."
lastmod: '2024-03-13T22:44:56.685815-06:00'
model: gpt-4-0125-preview
summary: "T\xE4ss\xE4 on miten heit\xE4t lainausmerkit nurkkaan Luassa."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Kuinka:
Tässä on miten heität lainausmerkit nurkkaan Luassa:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hei, Maailma!"'))     -- Hei, Maailma!
print(remove_quotes("'Näkemiin, Lainausmerkit!'"))  -- Näkemiin, Lainausmerkit!
```

Bingo! Nuo lainausmerkit katosivat kuin sukat kuivausrummussa.

## Syväsukellus
Ihmiset ovat puhdistaneet merkkijonoja lainausmerkeistä siitä lähtien, kun kieli on pystynyt käsittelemään tekstiä, mikä on melkein ikuisesti. Luassa `gsub`-funktio tekee raskaan työn, käyttäen malleja kuin skalpelia poistaakseen lainausmerkit. Vaihtoehtoja? Tietenkin voisit käyttää säännöllisiä lausekkeita (regex) kielissä, jotka tukevat sitä, tai kirjoittaa oman silmukan, joka käy läpi jokaisen merkin (haukotus, mutta hei, se on sinun aikaasi).

Luas Mallien vastaavuus antaa sinulle regex-lite kokemuksen ilman, että tarvitsee tuoda koko kirjastoa. Karet-merkki (`^`) ja dollarimerkki (`$`) vastaavat merkkijonon alkua ja loppua; `%p` vastaa mitä tahansa välimerkkiä. Pudotettuamme johtavan ja seuraavan välimerkin, me otamme kaiken muun talteen `(.*),` ja korvaamme koko vastaavuuden tuolla kiinniotto-ryhmällä käyttäen `" %1"`.

Muista, että Luas mallien vastaavuus ei ole yhtä tehokas kuin täysiveriset regex-moottorit – esimerkiksi se ei voi laskea eikä perääntyä. Tämä yksinkertaisuus on sekä siunaus että kirous, riippuen mitä lainausmerkkejä olet kahmimassa ja missä ne piileskelevät.

## Katso Myös
Sukella syvemmälle Luas mallien vastaavuuteen PiL (Programming in Lua) -kirjan avulla: http://www.lua.org/pil/20.2.html

Puhdasverisen eleganssin vuoksi, katso miten muut kielet tekevät sen vertailun vuoksi, alkaen Pythonin `str.strip`: https://docs.python.org/3/library/stdtypes.html#str.strip
