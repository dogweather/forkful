---
title:                "Pienempien merkkijonojen erottaminen"
html_title:           "Gleam: Pienempien merkkijonojen erottaminen"
simple_title:         "Pienempien merkkijonojen erottaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erottaa osamerkkijonoja? Substringien erottelu on erittäin hyödyllinen ohjelmoinnin tekniikka, joka voi auttaa sinua tehokkaammin käsittelemään tekstejä ja merkkijonoja.

## Kuinka tehdä

```Gleam
let teksti = "Hei maailma!"
let alku = 0
let loppu = 3
let osa = teksti[alku..loppu]
text.println(osa) // tulostaa "Hei"
```

Koodiesimerkissä näytämme, kuinka alusta ja lopusta voi määrittää osamerkkijonon erottelun avulla. Voit myös käyttää negatiivisia lukuja, kuten `-1`, jolloin aloitat lopusta päin. Voit myös käyttää `text.count_chars` -funktiota saadaksesi merkkien määrän.

## Syventävä tutkimus

Substringien erottelu toimii myös silloin, kun määrittelet vain toisen rajan. Esimerkiksi jos haluat erottaa osan merkkijonosta loppuun asti, voit määrittää vain alun kohdaksi `teksti[2..]`. Voit myös käyttää `text.trim` -funktiota poistaaksesi ylimääräiset välilyönnit sekä alusta että lopusta.

## Katso myös

- Gleamin tekstien käsittelydokumentaatio (https://gleam.run/lib/text.html)
- Gleam-oppaat ja tutoriaalit (https://gleam.run/learn/)