---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hahmojen poistaminen tietyn mallin mukaisesti tarkoittaa sellaisten merkkien poistamista, jotka täyttävät tietyn ehdon tai säännön. Ohjelmoijat tekevät tämän yleensä siistimään ja parantamaan dataa.

## Kuinka Näin:

Voit poistaa merkit tietyn mallin mukaan Lua-ohjelmointikielellä käyttäen gsub-metodia. Esimerkissämme poistetaan kaikki numerot merkkijonosta:

```Lua
merkkijono = "abcd1234"
muokattu_merkkijono = merkkijono:gsub("%d", "")
print(muokattu_merkkijono)  -- Output: "abcd"
```

## Syvällisemmin:

Historiallisessa kontekstissa mallien mukaisten merkkien poistaminen on ollut olennainen osa ohjelmointikielejä vuosikymmenten ajan. Luan gsub-metodi, joka otettiin käyttöön jo varhaisessa versiossa, on peräisin Perl-ohjelmointikielestä.

Vaihtoehtoisina lähestymistapoina voidaan käyttää erilaisia mallikirjastoja, kuten lpeg tai rex, jotka tarjoavat laajempia tai erilaisia toiminnallisuuksia.

Gsub-metodin toiminta perustuu siihen, että se etsii merkkijonosta mallin määräämiin kohtiin ja korvaa nämä osat toisella merkkijonolla (tässä tapauksessa tyhjällä merkkijonolla, "" pois lukien mallia vastaavat merkit)

## Katso Myös:

- Lua:n virallinen dokumentaatio gsub-metodista: http://www.lua.org/manual/5.1/manual.html#pdf-string.gsub  
- Rex-kirjastoja, erityisesti PCRE-mallijärjestelmää varten: http://lualibrary.fandom.com/wiki/REX  
- Huipputehokas mallitoteutus käyttäen LPEG-kirjastoa: http://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html