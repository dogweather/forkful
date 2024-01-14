---
title:                "Gleam: Merkkijonon muuntaminen isoiksi kirjaimiksi"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää Gleamia merkkijonojen muokkaamiseen? Esimerkiksi monissa ohjelmointiprojekteissa tarvitaan pientä mutta tärkeää ominaisuutta, kuten merkkijonojen muuttamista isoiksi kirjaimiksi.

## Kuinka

Gleamilla tämän ominaisuuden toteuttaminen on erittäin helppoa. Käytä vain sisäänrakennettua `String.capitalize` -funktiota ja anna sille haluamasi merkkijono parametrina. Katso esimerkki alla olevista koodilohkoista ja tulos:

```Gleam
let result = String.capitalize("hei")
io.format("Tulos: {}", [result]) // Tulostaa "Tulos: Hei"
```

Jos haluat muuttaa merkkijonon kaikki kirjaimet isoiksi, voit käyttää `String.to_uppercase` -funktiota. Esimerkki käyttäen Gleamin `List` -moduulin `map` -funktiota:

```Gleam
let string = "tämä on pienillä kirjaimilla"
let uppercase_string = List.map(String.to_uppercase, String.to_list(string))
let result = String.from_list(uppercase_string)
io.format("Tulos: {}", [result]) // Tulostaa "Tulos: TÄMÄ ON PIENILLÄ KIRJAIMILLA"
```

## Syvällinen sukellus

Miten Gleamin `String.capitalize` -funktio toimii taustalla? Se käyttää `String.to_uppercase` -funktiota muuttaakseen merkkijonon ensimmäisen kirjaimen isoksi ja jättää muut kirjaimet ennalleen.

Lisäksi Gleam käyttää Unicode-standardia merkkijonojen käsittelyssä, joten se osaa käsitellä monia erikoismerkkejä ja erilaisia kirjoitusjärjestelmiä oikein.

## Katso myös

- [Gleam string-moduuli](https://gleam.run/modules/string/)
- [Gleam Unicode-tuki](https://gleam.run/learn/glossary/#unicode)