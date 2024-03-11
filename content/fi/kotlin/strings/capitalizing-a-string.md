---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:48.804351-07:00
description: "Merkkijonon alkukirjaimen muuttaminen suureksi ohjelmoinnissa tarkoittaa\
  \ merkkijonon ensimm\xE4isen merkin muuttamista suuraakkoseksi, jos se ei jo ole,\
  \ mik\xE4\u2026"
lastmod: '2024-03-11T00:14:30.457712-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon alkukirjaimen muuttaminen suureksi ohjelmoinnissa tarkoittaa\
  \ merkkijonon ensimm\xE4isen merkin muuttamista suuraakkoseksi, jos se ei jo ole,\
  \ mik\xE4\u2026"
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon alkukirjaimen muuttaminen suureksi ohjelmoinnissa tarkoittaa merkkijonon ensimmäisen merkin muuttamista suuraakkoseksi, jos se ei jo ole, mikä on hyödyllistä käyttäjäsyötteiden muotoilussa tai tekstin näyttämisessä käyttöliittymässä standardoidummalla tai ihmisläheisemmällä tavalla. Ohjelmoijat suorittavat tämän toimenpiteen varmistaakseen tietojen johdonmukaisuuden tai täyttääkseen tiettyjä muotoiluvaatimuksia ohjelmistosovelluksissaan.

## Kuinka:

Kotlinissa merkkijonoja voidaan muuttaa alkamaan suuraakkosella käyttämällä standardikirjaston funktioita ilman kolmannen osapuolen kirjastoja. Kotlinin lähestymistapa merkkijonojen käsittelyssä tekee näistä toimenpiteistä suoraviivaisia ja suppeita.

### Koko merkkijonon muuttaminen suuraakkosiksi:

```kotlin
val viesti = "hei maailma!"
val isoViesti = viesti.uppercase()

println(isoViesti) // Tuloste: HEI MAAILMA!
```

### Vain ensimmäisen merkin muuttaminen suuraakkoseksi:

Kotlinin versiossa 1.5 `capitalize()`-funktio on vanhentunut ja korvattu yhdistelmällä `replaceFirstChar` ja lambda, joka tarkistaa, onko kyseessä pieni kirjain muuttaakseen sen suuraakkoseksi.

```kotlin
val tervehdys = "hei maailma!"
val isoTervehdys = tervehdys.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(isoTervehdys) // Tuloste: Hei maailma!
```

Tämä lähestymistapa säilyttää loput lauseesta alkuperäisessä muodossaan muuttaen ainoastaan ensimmäisen kirjaimen suuraakkoseksi.
