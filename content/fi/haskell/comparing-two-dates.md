---
title:    "Haskell: Kahta päivämäärää vertailemalla"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää?

Päivämäärien vertaileminen on tärkeä osa ohjelmointia monessa tilanteessa. Se auttaa meitä muun muassa löytämään tietyn päivämäärän välissä olevia päiviä, vertailemaan tapahtumien aikajärjestystä ja laskemaan päivämäärien välisiä eroja. Haskell tarjoaa helpon ja tehokkaan tavan vertailla päivämääriä.

# Miten vertailla kahta päivämäärää Haskellilla?

Haskellissa päivämäärien vertaileminen tapahtuu DateTime-moduulin avulla. Aloita tuomalla moduuli ohjelmaasi ```import Data.Time```. Seuraavaksi voit luoda kaksi päivämäärämuuttujaa käyttämällä ```getCurrentTime```-funktiota ja muuntamalla sen päivämääräksi käyttämällä ```utctDay```-funktiota.

```Haskell
currentDate <- getCurrentTime
let date1 = utctDay currentDate
let date2 = utctDay currentDate
```

Huomaa, että tässä esimerkissä molemmat päivämäärät on asetettu samaksi. Voit vaihdella niitä tarpeesi mukaan.

Seuraavaksi käytä ```compare```-funktiota vertailemaan päivämääriä keskenään. Tämä funktio palauttaa tilauksen mukaisen arvon, joka kertoo, kumpi päivämäärä on suurempi.

```Haskell
let result = compare date1 date2
```
Tämä tarkoittaa sitä, että jos ```result``` on ```LT```, ensimmäinen päivämäärä on pienempi kuin toinen. Jos se on ```GT```, ensimmäinen päivämäärä on suurempi kuin toinen ja jos se on ```EQ```, molemmat päivämäärät ovat samat.

Voit myös käyttää muita vertailufunktioita, kuten ```subtractDays``` ja ```addDays``` laskeaksesi päivämäärien välisen eron.

# Syvempää tietoa päivämäärien vertailusta

Haskelliin sisältyy myös muita hyödyllisiä funktioita päivämäärien vertailuun, kuten ```diffDays```, joka laskee päivien määrän kahden päivämäärän välillä, sekä ```isSameDay```, joka tarkistaa ovatko kaksi päivämäärää samat.

On myös tärkeää muistaa aina käsitellä päivämäärät oikeassa muodossa, jotta vertailu toimii oikein. Esimerkiksi, jos päivämäärä on merkitty muodossa ```YYYY-MM-DD```, se ei välttämättä toimi, jos käytät erilaista muotoa, kuten ```DD/MM/YYYY```.

# Katso myös

- Haskellin Datetime-moduuli: https://hackage.haskell.org/package/datetime
- Ohjeet päivämäärien käsittelystä Haskelissa: https://wiki.haskell.org/Date_and_time

Kiitos lukemisesta ja onnea päivämäärien vertailun kanssa!