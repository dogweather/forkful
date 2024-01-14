---
title:                "Haskell: Virheenkorjaustulostuksen tulostaminen"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi ohjelmoijat voivat haluta tulostaa debug-viestejä koodiinsa. Usein se auttaa selkeyttämään koodin suoritusta ja havaitsemaan mahdollisia virheitä.

## Kuinka

Saatat tarvita debug-viestejä tulostamaan, kun koodissasi on monimutkaisia toimintoja tai toiston sisällä tapahtuvia laskelmia. Voit tulostaa viestit käyttämällä `Debug.Trace` moduulia. Käytä `trace` funktiota ja anna sille viesti ja arvo, joka haluat tulostaa.

```Haskell
import Debug.Trace

x = 5 * trace "Lasketaan x:n arvo" z
  where z = 3 + trace "Lasketaan z:n arvo" 2
```

Tulostettu tulos:

```
Lasketaan z:n arvo
Lasketaan x:n arvo
25
```

## Syvempi sukellus

Voit myös käyttää `traceShow` funktiota tulostamaan muuttujien arvoja haluamallasi tavalla. Esimerkiksi, jos haluat nähdä lista- ja tuple-tietorakenteiden sisällön, voit käyttää `show` funktiota.

```Haskell
import Debug.Trace

x = traceShow "Tämä on x" [1, 2, 3]
y = traceShow "Tämä on y" ("Hei", "Heippa")

-- Output:
-- Tämä on y
-- ("Hei","Heippa")
-- Tämä on x
-- [1,2,3]
```

Voit myös käyttää `traceShowId` funktiota, joka toimii samalla tavalla kuin `traceShow`, mutta ei lisää ylimääräisiä painikkeita tulosteeseen.

```Haskell
import Debug.Trace

x = traceShowId "Tämä on x" 5

-- Output:
-- Tämä on x
-- 5
```

Muista kuitenkin, että debug-viestien tulostaminen vaikuttaa suorituskykyyn, joten käytä niitä harkitusti ja poista ne lopullisesta koodista.

## Katso myös

- [Haskellin Debug.Trace dokumentaatio](https://hackage.haskell.org/package/base-4.7.0.2/docs/Debug-Trace.html)
- [Haskell debug-viestien käyttöönotto Stack-komennoilla](https://www.digitalocean.com/community/tutorials/how-to-debug-in-haskell-with-stack-trace)
- [Haskell debuggausghidit (englanniksi)](https://williamyaoh.com/posts/2017-07-08-debugging-guide.html)