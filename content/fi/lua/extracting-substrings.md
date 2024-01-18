---
title:                "Alistringin erottaminen"
html_title:           "Lua: Alistringin erottaminen"
simple_title:         "Alistringin erottaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Lua on ohjelmointikieli, jota käytetään monenlaisiin tarkoituksiin. Yksi sen tärkeimmistä ominaisuuksista on kyky käsitellä merkkijonoja. Merkkijonot ovat yksinkertaisesti tekstiä, ja niissä voi olla paljon erilaisia tietoja. Yksi tapa käsitellä merkkijonoja Lua:ssa on erottaa osia merkkijonosta ja saada niistä alimerkkijonoja.

Kun ohjelmoijat tarvitsevat tiettyjä tietoja merkkijonosta, esimerkiksi puhelinnumeron tai sähköpostiosoitteen, he voivat käyttää alimerkkijonojen erottamista. Tämä auttaa heitä käsittelemään ja käyttämään tietoja tarvittavalla tavalla.

# Kuinka:
Lua tarjoaa monia tapoja erottaa alimerkkijonoja merkkijonosta. Yksi tapa on käyttää string.sub() -funktiota, joka hyödyntää kahta parametria: merkkijonoa sekä aloitus- ja lopetusindeksejä. Alla on esimerkki koodista ja sen tulosteesta:

```Lua
local teksti = "Tämä on esimerkkiteksti."

-- Tulostaa alimerkkijonon, joka alkaa indeksistä 5 ja päättyy 11
print(string.sub(teksti, 5, 11))

-- => on esim
```

Toinen tapa tehdä sama asia on käyttää merkkijonon pisteoperaattoria. Alla olevassa esimerkissä käytämme myös string.len() -funktiota, joka palauttaa merkkijonon pituuden.

```Lua
local teksti = "Tämä on esimerkkiteksti."

-- Tulostaa alimerkkijonon, joka alkaa indeksistä 5 ja jatkuu loput merkkijonosta
print(teksti:sub(5))

-- => on esimerkkiteksti.
```

# Syvään sukellus:
Osoittaaksemme tarkemmin, kuinka tärkeä alimerkkijonon erottaminen on, voimme katsoa taaksepäin Lua:n alkuperään. Alun perin Lua kehitettiin alustaksi PiL (Programming in Lua) -kirjalle, joka käsittelee tarkasti merkkijonoja ja niiden käsittelyä. Tarkoituksena oli tarjota helppokäyttöinen ohjelmointikieli, joka pystyy käsittelemään monimutkaisiakin merkkijonoja.

On myös muita tapoja erottaa alimerkkijonoja kuin edellä mainittuja. Näihin kuuluu mm. string.match() -funktio, joka käyttää säännöllisiä lausekkeita alimerkkijonon etsimiseen, sekä gmatch ja gsub -funktiot, jotka ovat hyödyllisiä sukellukseen ja korvaamiseen merkkijonossa.

# Katso myös:
* [Lua Reference Manual - String Manipulation](https://www.lua.org/manual/5.4/manual.html#6.4)
* [Programming in Lua](https://www.lua.org/pil/20.html) book by Roberto Ierusalimschy