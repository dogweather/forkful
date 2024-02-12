---
title:                "Päivämäärän jäsentäminen merkkijonosta"
aliases: - /fi/vba/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:59:07.618631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsennys merkkijonosta Visual Basic for Applications (VBA) -ohjelmistossa tarkoittaa tekstin, joka esittää päivämäärää, muuntamista päivämäärätyyppiseksi tiedoksi. Ohjelmoijat tekevät tämän käsitelläkseen päivämääriä tehokkaammin sovelluksissaan, esimerkiksi vertailuja, laskelmia tai muotoiluja varten.

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
