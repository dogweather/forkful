---
title:    "Haskell: Kirjoittaminen standardi virheeseen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Haskellissa on useita tapoja kirjoittaa tietoa ulos ohjelmasta. Yksi tärkeä tapa on kirjoittaa tietoa standardivirheelle (standard error). Tässä blogikirjoituksessa käymme läpi, miksi kirjoittaminen standardivirheelle voi olla hyödyllistä ja kuinka se tehdään Haskellissa.

## Kuinka tehdä se

Kirjoittaminen standardivirheelle on yksinkertaista Haskellissa. Käytämme siihen `putStrLn`-funktiota ja välittäämme sille virheilmoituksen merkkijonona.

```Haskell
putStrLn "Tämä on virheilmoitus standardivirheelle!"
```

Kun ohjelma suoritetaan, tämä merkkijono tulostuu standardivirheelle. Voimme myös yhdistää useita merkkijonoja `++`-operaattorilla ja välittää ne yhtenä virheilmoituksena.

```Haskell
putStrLn $ "Virhe: " ++ "Tämä on virheilmoitus."
```

Tämä tulostaa "Virhe: Tämä on virheilmoitus." standardivirheelle.

Voimme myös käyttää `fail`-funktiota toteuttamaan oman virheenkäsittelylogiikkamme. `fail` ottaa argumenttina virheilmoituksen ja palauttaa `IO`-tyypin arvon, joka sisältää kyseisen virheen.

```Haskell
fail "Tämä on virheilmoitus."
```

## Syvällisempi sukellus

Mutta miksi kirjoittaa tietoa standardivirheelle? Usein haluamme erottaa normaalit tulostukset ja virheilmoitukset toisistaan. Virheilmoituksia käytetään usein silloin, kun jotain on mennyt pieleen ohjelman suorituksessa. Tällöin virheen ilmoittaminen standardivirheelle erottaa sen muista tulostuksista ja helpottaa virheen löytämistä.

Lisäksi kirjoittaminen standardivirheelle on hyödyllistä silloin, kun haluamme käsitellä virheitä jollakin tavalla. Voimme lukea standardivirhettä toisessa osassa ohjelmaa ja käsitellä virheet erikseen. Tämä auttaa meitä tekemään ohjelmistamme luotettavampia.

## Katso myös

[Täydellinen opas Haskellin tulostusominaisuuksiin](https://www.haskell.org/tutorial/io.html#output)

[Haskellin dokumentaatio standardovirhelogiikasta](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Error.html)