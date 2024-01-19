---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hakeminen ja korvaaminen on tekstissä olevien merkkijonojen löytäminen ja vaihtaminen. Ohjelmoijat tekevät tämän, koska se helpottaa koodin muokkaamista ja virheiden korjaamista.

## Näin teet:

Lua-ohjelmoinnissa hakeminen ja korvaaminen on helppoa käyttäen `gsub`-funktiota. Tässä on esimerkki:

```Lua
korvattavaTeksti = "Hello, World!"
korvattava = "Hello"
korvaaja = "Hei"

korjattuTeksti = korvattavaTeksti:gsub(korvattava, korvaaja)
print(korjattuTeksti)
```

Esimerkkikoodin tuloste:

```
Hei, World!
```

## Syvällisemmin:

(1) Historiallinen tausta: Alkuperäinen `gsub`-funktio oli osa Lua-kielen alkuperäistä luomista 1990-luvulla. Se oli yksi ensimmäisistä kieleen lisätyistä ominaisuuksista, jotka tekevät tekstikäsittelystä helpompaa.

(2) Vaihtoehdot: Lua tarjoaa myös `sub`-funktion, joka voi korvata tekstiä vain kerran, kun taas `gsub` voi korvata merkkijonoja useita kertoja.

(3) Toteutustiedot: `gsub`-funktio toimii käymällä läpi merkkijonon alusta loppuun ja korvaa löydetyt sana tai sanat määriteltyjen sääntöjen mukaan. Se palauttaa uuden merkkijonon, jossa korvaukset on tehty, jättäen alkuperäisen merkkijonon muuttumattomaksi.

## Katso myös:

Lisätietoja Lua-kielen tekstikäsittelystä saat seuraavista lähteistä:

- Lua 5.3 manualin `string`-library-osio: https://www.lua.org/manual/5.3/manual.html#6.4
- Programming in Lua, Roberto Ierusalimschy: http://www.lua.org/pil/20.html
- Lua Users Wiki - String Library Tutorial: http://lua-users.org/wiki/StringLibraryTutorial