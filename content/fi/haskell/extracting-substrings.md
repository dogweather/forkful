---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Alistringien erottaminen tarkoittaa merkkijonon osien valitsemista ja käyttämistä. Tämä on hyödyllistä, kun haluamme tarkastella tai käyttää vain osaa datasta.

## Näin se toimii:

Haskellissa voimme käyttää erilaisia funktioita alistringien erottamiseksi. Tässä on muutama esimerkki:

```Haskell
-- 'take' ottaa n ensimmäistä merkkiä
take 5 "Hello, World!"    -- Palauttaa "Hello"

-- 'drop' jättää n ensimmäistä merkkiä pois
drop 7 "Hello, World!"    -- Palauttaa "World!"

-- 'splitAt' jakaa merkkijonon n kohdasta kahteen osaan
splitAt 5 "Hello, World!" -- Palauttaa ("Hello",", World!")
```

## Syvä sukellus:

Haskell otettiin käyttöön 1990-luvulla, ja sen standardikirjasto tarjoaa erilaisia tapoja merkkijonojen käsittelyyn. Yllä käytetyt 'take', 'drop' ja 'splitAt' -funktiot ovat osa tätä kirjastoa.

Vaihtoehtoisesti voimme käyttää 'substring' -funktiota 'Data.Text' kirjastosta, joka tarjoaa tehokkaampaa merkkijonojen käsittelyä suurille datamassoille.

Näiden funktioiden implementointi Haskellissa perustuu sen 'lazy-evaluation' -periaatteelle, joka mahdollistaa tehokkaan suorituskyvyn ja muistinkäytön.

## Katso myös:

Linkkejä liittyviin lähteisiin:

- Merkkijonojen käsitteleminen Haskellin peruskirjastossa: https://haskell-lang.org/tutorial/string-types
- Data.Text kirjaston dokumentaatio: http://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html