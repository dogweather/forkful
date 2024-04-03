---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:48.804351-07:00
description: "Kuinka: Kotlinissa merkkijonoja voidaan muuttaa alkamaan suuraakkosella\
  \ k\xE4ytt\xE4m\xE4ll\xE4 standardikirjaston funktioita ilman kolmannen osapuolen\
  \ kirjastoja.\u2026"
lastmod: '2024-03-13T22:44:56.514422-06:00'
model: gpt-4-0125-preview
summary: "Kotlinissa merkkijonoja voidaan muuttaa alkamaan suuraakkosella k\xE4ytt\xE4\
  m\xE4ll\xE4 standardikirjaston funktioita ilman kolmannen osapuolen kirjastoja."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

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
