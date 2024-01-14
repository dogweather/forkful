---
title:    "Haskell: Tämänhetkisen päivämäärän saaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Miksi Muodostaa Hästäkaltaiseen Päivämääriä

Haskell on monipuolinen ohjelmointikieli, joka tarjoaa laajan valikoiman ominaisuuksia ja työkaluja. Yksi näistä hyödyllisistä toiminnoista on nykyisen päivämäärän hankkiminen. Tässä blogikirjoituksessa tutustumme siihen, miten kannattaa käyttää Haskellia nykyisen päivämäärän saamiseksi ja miksi se voi olla hyödyllistä.

## Kuinka Tehdä Se

```haskell
import Data.Time

main = do
  t <- getCurrentTime
  let d = utctDay t
  putStrLn $ "Nykyinen päivämäärä on: " ++ show d
```

Tämä yksinkertainen ohjelma hakee nykyisen päivämäärän ja tulostaa sen konsoliin. Ensimmäinen rivi tuo Data.Time -kirjaston, joka sisältää tarvittavat toiminnot päivämäärän käsittelyyn. Sitten `getCurrentTime` -funktio hakee nykyisen ajan ja tallentaa sen muuttujaan `t`. Seuraavassa rivissä käytämme `utctDay` -funktiota muuntamaan UTC (koordinoitu yleinaikaa) -ajan `Day` -tyypiksi, joka sisältää vain päivämäärän ilman aikatietoa. Lopuksi tulostamme päivämäärän konsoliin käyttämällä `putStrLn` -funktiota.

```bash
Nykyinen päivämäärä on: 2021-07-26
```

Tämä esimerkki toimii nykyisen päivämäärän saamiseksi vuoden, kuukauden ja päivän tarkkuudella. Mutta mitä jos haluamme saada tarkempaa tietoa, kuten tunteja, minuutteja ja sekunteja? Katsotaanpa, kuinka se tehdään:

```haskell
import Data.Time
import Data.Time.Format

main = do
  t <- getCurrentTime
  let d = formatTime defaultTimeLocale "%H:%M:%S" t
  putStrLn $ "Nykyinen aika on: " ++ show d
```

Tässä esimerkissä olemme tuoneet toisen kirjaston, Data.Time.Format, joka tarjoaa lisää toimintoja ajanmuotoilulle. Käytämme `formatTime` -funktiota muuttamaan ajanmuodon haluamaksemme. Ensimmäinen parametri `defaultTimeLocale` kertoo muotoilun käyttämään oletuskieltä ja asetuksia, tässä tapauksessa annamme vain kellonajan ilman päivämäärää. Lopuksi tulostamme ajan konsoliin samalla tavalla kuin ensimmäisessä esimerkissä.

```bash
Nykyinen aika on: 19:30:15
```

## Syventävä Tarkastelu

Nyt kun tiedämme, kuinka saada nykyinen päivämäärä tai aika Haskellilla, voimme tutkia tarkemmin niitä toimintoja, joita käytimme. Data.Time -kirjastoon kuuluu paljon muita toimintoja, kuten erilaisia muotoilumahdollisuuksia tai aikavyöhykkeiden käsittelyä. Voit tutustua niihin virallisessa dokumentaatiossa: https://hackage.haskell.org/package/time.

On myös arvokasta huomata, että `getCurrentTime` -funktio palauttaa datatyypin `UTCTime`, joka sisältää sekä päivämäärän että ajan. Muuntamalla se eri tavoin voit saada tarkempia tietoja, kuten viikonpäivän tai vuodenajat.

# Katso Myös

- https://haskell-lang.org/
- https://hackage.haskell.org