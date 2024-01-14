---
title:    "Haskell: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Päivämäärän muuntaminen merkkijonoksi on tärkeää, jotta voit esittää päivämääriä selkeässä, universaalissa muodossa. Tämä on erityisen hyödyllistä, kun jaat tietoja useiden käyttäjien kanssa tai tallennat niitä tietokantoihin.

## Ohjeet

Käyttämällä Haskellia voit helposti muuntaa päivämäärän merkkijonoksi yksinkertaisella metodilla.

```Haskell
import Data.Time.Format
import Data.Time.LocalTime

dateToString :: LocalTime -> String
dateToString date = formatTime defaultTimeLocale "%d/%m/%Y" date
```

Tämä koodi käyttää `Data.Time.Format` -moduulia, joka tarjoaa `formatTime` -funktion päivämäärän muuntamiseen merkkijonoksi. Lisäksi käytämme `Data.Time.LocalTime` -moduulia saadaksemme `LocalTime` -tyypin, joka edustaa päivämäärää ja aikaa.

Seuraavaksi kutsumme `dateToString` -funktiota antamalla sille haluamamme `LocalTime` -arvon. Esimerkiksi, jos haluamme muuntaa nykyisen päivämäärän merkkijonoksi, voimme käyttää `ZonedTime` -tyyppiä ja `LocalTime` -funktiota:

```Haskell
import Data.Time.Zones.All

myDate :: ZonedTime
myDate = localTimeToZonedTime utc (LocalTime (fromGregorian 2021 8 18) (TimeOfDay 10 30 0))

dateToString myDate
```

Tulos olisi seuraava: `18/08/2021`

## Syvällinen sukellus

`formatTime` tarjoaa useita vaihtoehtoja päivämäärän muotoiluun, joten sinun kannattaa lukea tarkempi dokumentaatio [täältä](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html).

Lisäksi, voit muuttaa käytettyä muotoa `defaultTimeLocale` -muuttujan avulla. Siinä on joitakin valmiina tarjolla, mutta voit myös luoda oman muodostimen tarpeidesi mukaan.

## Katso myös

- [Data.Time.Format dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Data.Time.LocalTime dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
- [Data.Time.Zones.All dokumentaatio](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Zones-All.html)