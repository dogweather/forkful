---
title:                "Haskell: Tiedostoon standardivirheen kirjoittaminen"
simple_title:         "Tiedostoon standardivirheen kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardivirtaan

Kirjoittaminen standardivirtaan on tärkeä osa Haskell-ohjelmointia, jota jokaisen kehittäjän tulisi osata. Se mahdollistaa virheiden ja toimintatietojen näyttämisen suoraan terminaaliin, mikä helpottaa ohjelman käytettävyyttä ja korjaamista.

## Miten kirjoittaa standardivirtaan

Kirjoittaminen standardivirtaan on helppoa Haskellissa. Käytä `System.IO` -moduulia ja `stderr` -funktiota tulostamaan haluamasi viesti. Esimerkiksi:

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Tervetuloa Haskell-maailmaan!"
```

Voit myös ottaa käyttöön `Control.Monad` -moduulin avulla virhekäsittelyn ja tulostaa virheviestisi `stderr`-virtaan käyttäen `when`-funktiota. Esimerkiksi:

```Haskell
import System.IO
import Control.Monad

main = do
    let x = 10
    when (x <= 0) $ hPutStrLn stderr "Virhe: x ei voi olla negatiivinen!"
```

Näiden esimerkkien tulostus näyttäisi tältä:

```shell
Tervetuloa Haskell-maailmaan!
```
tai

```shell
Virhe: x ei voi olla negatiivinen!
```

Ei ole käytännöllistä kirjoittaa kaikkia virheviestejä `stdout`-virtaan, sillä silloin ne sekoittuisivat ohjelman normaalin tulostuksen kanssa. Siksi on tärkeää käyttää `stderr`-virtaa virheiden ja toimintatietojen näyttämiseen.

## Syventävä tieto kirjoittamisesta standardivirtaan

On hyvä käytäntö kirjoittaa siististi jäsenneltyjä ja informatiivisia virheviestejä, jotka auttavat käyttäjää ymmärtämään ongelman ja korjaamaan sen. Voit myös halutessasi käyttää `hPrintf`-funktiota muotoilun helpottamiseksi. Esimerkiksi:

```Haskell
import System.IO
import Text.Printf

main = do
    let x = 5
    let y = 0
    when (y == 0) $ hPrintf stderr "Virhe: %d ei voi olla jakajan arvo!" y
```

Tämä tulostaisi seuraavan virheilmoituksen:

```shell
Virhe: 0 ei voi olla jakajan arvo!
```

Nyt kun tiedät, kuinka kirjoittaa standardivirtaan, voit parantaa ohjelmasi käytettävyyttä ja helpottaa sen virheiden korjaamista.

## Katso myös

- [Haskell-dokumentaatio - System.IO-moduuli](https://www.haskell.org/onlinereport/io.html#etag-haskell-document-filtering-data)
- [Haskell-dokumentaatio - Text.Printf-moduuli](https://www.haskell.org/onlinereport/standard-prelude.html#t%3APrintfType)
- [Haskell Wikibooks - Virhekäsittelystä](https://en.wikibooks.org/wiki/Haskell/Error_handling)