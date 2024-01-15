---
title:                "Tietokoneohjelmointi: Kirjoittaminen standardivirheeseen"
html_title:           "Haskell: Tietokoneohjelmointi: Kirjoittaminen standardivirheeseen"
simple_title:         "Tietokoneohjelmointi: Kirjoittaminen standardivirheeseen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen vakiokäyttövirheestä (standard error) on tärkeä osa tehokasta Haskell-ohjelmointia. Se auttaa kehittäjiä tunnistamaan ja ratkaisemaan virheitä nopeasti ja parantamaan ohjelmien suorituskykyä.

## Kuinka

Voit kirjoittaa vakiokäyttövirheeseen käyttämällä `hPutStrLn` -funktiota ja `stderr` -moduulia. Alla on esimerkki koodista, joka kirjoittaa merkkijonon vakiokäyttövirheeseen ja tulostaa sen samalla konsoliin:

 ```Haskell
import System.IO

main = do
 hPutStrLn stderr "Tämä on vakiokäyttövirhe"
 putStrLn "Tämä tulostetaan konsoliin"
 ```

Tämän koodin ulostulo on seuraava:

```haskell
Tämä on vakiokäyttövirhe
Tämä tulostetaan konsoliin
```

## Syväsukellus

Haskellin `hPutStrLn` -funktio ei ole ainoa tapa kirjoittaa vakiokäyttövirheeseen. Voit myös käyttää `hPutStr` -funktiota, joka tulostaa merkkijonon ilman rivinvaihtoa. Lisäksi voit käyttää `stderr` -moduulin sijasta `stderr` -arvoa. Alla on esimerkki koodista, joka käyttää näitä vaihtoehtoja:

```Haskell
import System.IO

main = do
 hPutStr stderr "Tämä on "
 hPutStrLn stderr "vakiokäyttövirhe"
 putStrLn "Tämä tulostetaan konsoliin"
```

Tämän koodin ulostulo on seuraava:

```haskell
Tämä on vakiokäyttövirhe
Tämä tulostetaan konsoliin
```

## Katso myös

- [Haskellin virallinen dokumentaatio vakiokäyttövirheen kirjoittamisesta](https://www.haskell.org/hoogle/?hoogle=stderr)
- [Haskellin perusteet](https://github.com/bitemyapp/learnhaskell)
- [50 tärkeää Haskell-funktiota, jotka sinun tulisi tietää](https://wiki.haskell.org/Category:Standard_functions)