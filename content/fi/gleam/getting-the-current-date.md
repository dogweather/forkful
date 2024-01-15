---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Gleam: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet Gleam-kielen käyttäjä tai harkitset sen käyttöä, sinulla saattaa olla tarve saada nykyinen päivämäärä ohjelmassasi. Oli sitten kyse laskennasta tai käyttäjän näyttämästä ajasta, Gleam:ssa on helppo tapa saada nykyinen päivämäärä nopeasti ja tarkasti.

## Kuinka

Gleam:ssa nykyinen päivämäärä voidaan saada muutamassa yksinkertaisessa vaiheessa. Ensimmäisenä sinun täytyy tuoda Gleam:n `Time` kirjasto `import Time` komennolla.

Sitten voit käyttää `Time.now()` funktiota saadaksesi nykyisen päivämäärän. Käytämme `let` avainsanaa tallentaaksemme päivämäärän muuttujaan, ja voimme käyttää `io:format!` funktiota tulostamaan päivämäärän terveystiimiin.

```Gleam
import Time

let tanaan = Time.now()
io:format!("Tänään on ~p", [tanaan])
```

Kun ajamme tämän ohjelman, tulostuu jotain tällaista:

```
Tänään on ~p2021-07-22T18:32:00Z
```

Z-merkintä tulee `io:format` funktion käyttämästä `DateTime.string()` muotoilusta. Voit lukea lisää Gleam:n `lio` standardikirjastosta [täältä](https://gleam.run/modules/io/latest/).

## Syvemmälle

`Time.now()` funktio palauttaa nykyisen ajan UTC:na eli koordinoituna maailmanlaajuisena aikana. Jos haluat muuttaa tämän paikalliseksi ajaksi, voit käyttää `Time.local_now()` funktiota. Tämä palauttaa nykyisen ajan omassa aikavyöhykkeessäsi.

Voit myös käyttää `DateTime.to_unix()` funktiota muuntaaksesi päivämäärän Unix-aikaleimaksi tai `DateTime.to_rfc3339()` muuntaaksesi sen RFC 3339 formaattiin.

## Katso myös

- Gleam:n `Time` kirjasto [dokumentaatio](https://gleam.run/modules/time/latest/)