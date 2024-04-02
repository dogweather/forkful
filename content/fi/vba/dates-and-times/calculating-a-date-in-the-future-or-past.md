---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:14.736870-07:00
description: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen\
  \ liittyy p\xE4iv\xE4m\xE4\xE4r\xE4n m\xE4\xE4ritt\xE4miseen, joka on m\xE4\xE4\
  r\xE4tyn lukum\xE4\xE4r\xE4n p\xE4ivi\xE4, kuukausia tai vuosia pois\u2026"
lastmod: '2024-03-13T22:44:56.415456-06:00'
model: gpt-4-0125-preview
summary: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen liittyy\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4n m\xE4\xE4ritt\xE4miseen, joka on m\xE4\xE4r\xE4tyn\
  \ lukum\xE4\xE4r\xE4n p\xE4ivi\xE4, kuukausia tai vuosia pois\u2026"
title: "Tulevaisuuden tai menneisyyden p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## Mikä & Miksi?
Tulevaisuuden tai menneisyyden päivämäärän laskeminen liittyy päivämäärän määrittämiseen, joka on määrätyn lukumäärän päiviä, kuukausia tai vuosia pois annetusta päivämäärästä. Ohjelmoijat tarvitsevat usein tätä toiminnallisuutta automatisoidakseen muistutuksia, tilauksia, voimassaolon päättymispäiviä ja aikataulutustehtäviä erilaisissa sovelluksissa.

## Kuinka:
Visual Basic for Applications (VBA) -ohjelmassa ensisijainen funktio tulevaisuuden tai menneisyyden päivämäärien laskemiseen on `DateAdd()`. Tämä funktio lisää määritetyn aikavälin päivämäärään ja palauttaa uuden päivämäärän.

Tässä on perusesimerkki, jossa lisätään 10 päivää nykyiseen päivämäärään:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Lisää 10 päivää nykyiseen päivämäärään
Debug.Print futureDate ' Tulostaa jotain tällaista: 20.04.2023
```

Samoin, löytääksemme päivämäärän 10 päivää menneisyydessä:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Vähentää 10 päivää nykyisestä päivämäärästä
Debug.Print pastDate ' Tulostaa: 31.03.2023, olettaen että tänään on 10.04.2023
```

Nämä esimerkit ovat melko suoraviivaisia. Voit korvata `"d"`:n muiden aikavälikoodien kanssa, kuten `"m"` kuukausille ja `"yyyy"` vuosille, erityyppisten päivämäärälaskelmien suorittamiseen. Tässä on, miten saatat laskea päivämäärän yhden vuoden tulevaisuudessa:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Lisää 1 vuoden nykyiseen päivämäärään
Debug.Print nextYear ' Tulostaa: 10.04.2024, jos tänään on 10.04.2023
```

## Syväsukellus
`DateAdd`-funktio on ollut olennainen osa VBA:ta sen alusta lähtien, periytyen edeltäjästään BASIC:sta. Vaikka se tarjoaa yksinkertaisuuden lisätä tai vähentää aikavälejä päivämääristä, on tärkeää huomata, että VBA, mukaan lukien sen päivämäärän käsittelytoiminnot, eivät aina vastaa mukavuutta tai tehokkuutta, jotka löytyvät uudemmista ohjelmointikielistä.

Esimerkiksi modernit kielet kuten Python `datetime`-moduulin kanssa tai JavaScript kirjastoilla kuten `moment.js` ja `date-fns` tarjoavat intuitiivisempia ja tehokkaampia tapoja päivämäärien käsittelyyn. Nämä vaihtoehdot tarjoavat parempaa tukea lokalisoinnille, aikavyöhykkeille ja karkausvuosille, mikä voi tehdä niistä sopivampia sovelluksiin, jotka vaativat tarkkoja päivämäärälaskelmia globaalilla tasolla.

Kuitenkin Excel-makroille ja sovelluksille, jotka vaativat integraatiota Microsoft Office -ekosysteemin kanssa, VBA pysyy käytännöllisenä valintana. Suora pääsy Excel-tietoihin ja niiden manipulointi tarjoaa merkittävän edun. Lisäksi useimpiin perus päivämäärälaskelmiin, kuten aikatauluttamiseen ja muistutuksiin, `DateAdd()` VBA:ssa tarjoaa riittävän ja suoraviivaisen ratkaisun. Sen syntaksi on helppo käsittää uusille tulokkaille, ja sen integraatio laajempiin Office-sarjan sovelluksiin varmistaa sen relevanssin tietyissä käyttötapauksissa.

Yhteenvetona, vaikka vaihtoehtoiset ohjelmointikielet saattavat tarjota nykyaikaisempia lähestymistapoja päivämäärän laskentaan, `DateAdd()` VBA:ssa toimii todisteena kielen pysyvästä asemasta aloilla, joilla sitä eniten tarvitaan.
