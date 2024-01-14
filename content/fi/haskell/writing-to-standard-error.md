---
title:    "Haskell: Tietokoneohjelmointi: Standard errorin kirjoittaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardi virheestä on tärkeä ohjelmoinnin taito, joka auttaa sinua havaitsemaan ja korjaamaan virheitä koodissasi. Se myös auttaa parantamaan ohjelmasi laatua ja toimivuutta.

## Miten

Kirjoittaaksesi standardi virheeseen käytä `hPutStrLn` -funktiota ja määritä oikea virheviesti, esimerkiksi:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Virhe: Tämä on esimerkki virheilmoituksesta!"
```

Tämä tulostaisi seuraavan viestin standardi virheeseen: `Virhe: Tämä on esimerkki virheilmoituksesta!`

## Syväsukellus

Kirjoittaessasi standardi virheeseen, on tärkeää ottaa huomioon muutama asia. Ensinnäkin, varmista että käytät `System.IO` -moduulia ja tuo se osaksi koodiasi. Toiseksi, voit muokata virheviestiäsi käyttämällä `putStr` tai `putStrLn` -funktioita. Kolmanneksi, voit myös yhdistää `hPutStrLn` -funktion ja `error` -funktion yhdessä, jotta ohjelmasi keskeytyy, jos virhe ilmenee.

## Katso myös

- [System.IO - Moduulin dokumentaatio](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Virheiden käsittely Haskellissa](https://wiki.haskell.org/Error_handling)