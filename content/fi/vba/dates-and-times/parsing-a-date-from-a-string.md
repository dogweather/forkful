---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:07.618631-07:00
description: "Kuinka: VBA tarjoaa yksinkertaisen tavan j\xE4sent\xE4\xE4 merkkijono\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4ksi k\xE4ytt\xE4m\xE4ll\xE4 `CDate`-funktiota tai `DateValue`-funktiota.\
  \ On kuitenkin\u2026"
lastmod: '2024-03-13T22:44:56.411053-06:00'
model: gpt-4-0125-preview
summary: "VBA tarjoaa yksinkertaisen tavan j\xE4sent\xE4\xE4 merkkijono p\xE4iv\xE4\
  m\xE4\xE4r\xE4ksi k\xE4ytt\xE4m\xE4ll\xE4 `CDate`-funktiota tai `DateValue`-funktiota."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta"
weight: 30
---

## Kuinka:
VBA tarjoaa yksinkertaisen tavan jäsentää merkkijono päivämääräksi käyttämällä `CDate`-funktiota tai `DateValue`-funktiota. On kuitenkin olennaista, että merkkijono on tunnistettavassa päivämäärämuodossa.

Tässä on perusesimerkki `CDate`:n käytöstä:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Jäsennetty päivämäärä: "; parsedDate
End Sub
```

Jos suoritat tämän koodin, tuloste Välitön-ikkunassa (saatavilla VBA-editorissa `Ctrl+G`:n kautta) olisi:

```
Jäsennetty päivämäärä: 1.4.2023 
```

Vaihtoehtoisesti voit käyttää `DateValue`-funktiota, joka on erityisempi päivämäärille (jättäen huomiotta aikaosan):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "huhtikuu 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Jäsennetty päivämäärä käyttäen DateValue:a: "; parsedDate
End Sub
```

Tämän esimerkin tuloste näyttäisi samankaltaisesti Välitön-ikkunassa:

```
Jäsennetty päivämäärä käyttäen DateValue:a: 1.4.2023
```

Pidä mielessä, että jäsennyksen onnistuminen riippuu merkkijonon päivämäärämuodon vastaamisesta järjestelmän tai sovellusasetuksia.

## Syväsukellus
Sisäisesti, kun VBA jäsentää merkkijonon päivämääräksi, se käyttää Windows-käyttöjärjestelmän alueellisia asetuksia päivämäärämuodon tulkitsemiseen. Tämä on tärkeää ymmärtää, koska päivämäärämerkkijono, joka jäsentyy täydellisesti yhdellä järjestelmällä, saattaa aiheuttaa virheen toisessa, jos ne käyttävät erilaisia päivämäärä-/aika-asetuksia.

Historiallisesti päivämäärien käsittely on ollut yleinen virheiden lähde sovelluksissa, erityisesti kansainvälisesti käytettävissä. Tämä VBA:n riippuvuus alueellisista asetuksista on syy, miksi jotkut saattavat harkita vaihtoehtoja, kuten ISO 8601 -muotoa (esim. "VVVV-KK-PP") yksiselitteisen päivämäärän esitysmuodon ja jäsennyksen kannalta eri järjestelmissä. Valitettavasti VBA ei tue ISO 8601:ä natiivisti, ja manuaalinen jäsennys olisi tarpeen tiukkaa noudattamista varten.

Monimutkaisempiin päivämääräjäsennyksiin kuin mitä `CDate` tai `DateValue` pystyvät käsittelemään, tai järjestelmän paikalliset asetukset huomiotta jättävää johdonmukaista jäsennystä varten, ohjelmoijat saattavat turvautua mukautettuihin jäsennysfunktioihin. Nämä voivat sisältää päivämäärämerkkijonon jakamisen osiin (vuosi, kuukausi, päivä) ja päivämäärän muodostamisen käyttäen `DateSerial`-funktiota. Toiset saattavat valita voimakkaampia kieliä tai kirjastoja, jotka on suunniteltu kansainvälistymistä silmällä pitäen näitä tehtäviä varten.
