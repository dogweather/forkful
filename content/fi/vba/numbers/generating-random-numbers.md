---
aliases:
- /fi/vba/generating-random-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:28.218677-07:00
description: "Satunnaislukujen tuottaminen Visual Basic for Applications (VBA) -ohjelmissa\
  \ mahdollistaa prosessien, joissa on sattumanvaraisuuden tai muuttuvuuden\u2026"
lastmod: 2024-02-18 23:09:07.405789
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen Visual Basic for Applications (VBA) -ohjelmissa\
  \ mahdollistaa prosessien, joissa on sattumanvaraisuuden tai muuttuvuuden\u2026"
title: Sattumanvaraisten numeroiden generointi
---

{{< edit_this_page >}}

## Mikä ja miksi?

Satunnaislukujen tuottaminen Visual Basic for Applications (VBA) -ohjelmissa mahdollistaa prosessien, joissa on sattumanvaraisuuden tai muuttuvuuden elementtejä, kuten nopanheitot tai tietojen näytteistäminen, simuloimisen. Ohjelmoijat käyttävät näitä tekniikoita kehittääkseen malleja, pelejä tai simulaatioita, joissa ennustettavat tulokset olisivat epärealistisia tai vähemmän hyödyllisiä.

## Miten:

VBA:ssa käytetään `Rnd`-funktiota satunnaislukujen tuottamiseen. Oletusarvoisesti `Rnd` tuottaa yksittäisen tarkkuuden liukuluvun, joka on suurempi tai yhtä suuri kuin 0 ja pienempi kuin 1. Tässä on muutama askel ja esimerkki satunnaislukujen tehokkaaseen hyödyntämiseen:

1. **Yksinkertainen satunnaisluku:**
   Perussatunnaisluvun luomiseksi sinun tarvitsee vain kutsua `Rnd()`:

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' Satunnaisluku välillä 0 ja 1
       MsgBox randomNumber
   End Sub
   ```

2. **Siemenarvon asettaminen:**
   `Randomize`-lause alustaa satunnaislukugeneraattorin, mikä voi olla ratkaisevan tärkeää varmistettaessa erilaisia tuloksia joka kerta, kun VBA-koodisi suoritetaan:

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **Numeroiden tuottaminen tietyssä välissä:**
   Usein haluat satunnaisluvun tietyssä välissä. Tässä on miten tuottaa numero 1:n ja 100:n välillä:

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' Satunnaisluku välillä 1 ja 100
       MsgBox randomNumber
   End Sub
   ```

### Esimerkkituloste:
Suoritettuasi `RandomNumberInRange`, saatat nähdä viestilaatikon, joka näyttää numeron kuten `45`.

## Syventävä tarkastelu:

VBA:n `Rnd`-funktio, vaikka onkin helppokäyttöinen, tuottaa itse asiassa pseudo-satunnaislukuja deterministisen algoritmin perusteella. Tämä tarkoittaa, että sen tuottamat lukusekvenssit eivät ole täysin satunnaisia, mutta ne voivat usein riittää yleisiin tehtäviin, jotka tarvitsevat stokastisia prosesseja.

Historiallisesti satunnaislukujen tuottamiskyky VBA:ssa ulottuu Basicin varhaisiin versioihin, ja se on ajan myötä mukautunut sisältämään ominaisuuksia kuten `Randomize` parantaakseen satunnaisuutta alustamalla algoritmin lähtöpisteellä. Kuitenkin sovelluksiin, jotka vaativat korkeaa satunnaisuuden tasoa, kuten turvalliset kryptografiset operaatiot, VBA:n `Rnd` ei ehkä ole paras työkalu. Vaihtoehtoja, joissa käytetään vahvempia ohjelmointiympäristöjä tai kieliä, jotka on suunniteltu kryptografian mielessä pitäen, kuten Pythonin `secrets`-moduuli tai Javan `SecureRandom`, tulisi harkita.

Huolimatta sen rajoituksista, satunnaislukujen tuottamisen yksinkertaisuus ja saavutettavuus VBA:ssa jatkavat sen tekemistä arvokkaaksi työkaluksi laajalle valikoimalle kevyempiä sovelluksia, simulaatiotyötä ja opetustarkoituksiin.
