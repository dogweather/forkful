---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Tulostetaan virheenkorjaustietoa Haskellissa

## Mikä & Miksi?
Ohjelmoijat tulostavat virheenkorjaustietoa ymmärtääkseen ohjelmien toimintaa ja löytääkseen virheitä. Tämä auttaa korjaamaan koodin helpommin ja nopeammin.

## Miten
Haskellissa voit käyttää `Debug.Trace` -moduulia tulostaaksesi virheenkorjaustietoa. Tässä on esimerkki:

```Haskell 
import Debug.Trace

main = trace "Hei Maailma!" return ()
```

Kun ajat tämän ohjelman, se tulostaa `Hei Maailma!` -viestin ja ei tee muuta.

## Syvempi tutkiskelu
Historiallisesti, `Debug.Trace` -moduulin käyttö on ollut yleinen tapa tulostaa virheenkorjaustietoa Haskellissa, vaikka se on hankala koska rikkoo puhtaan funktionaalisuuden. 

Vaihtoehtoisesti, voit käyttää `System.IO.Unsafe` -moduulia. Mutta, tämäkin voi aiheuttaa väärinkäytöksiä, koska se rikkoo puhtaan funktionaalisuuden hahmotuksen.

Monet Haskell-kehittäjät suosivat lokeja debuggausprosessissa. Loggerit, kuten `monad-logger` tai `fast-logger`, ovat suosittuja, koska ne eivät riko puhtaita funktioita.

## Katso myös
Tärkeitä linkkejä Haskell-ohjelmointijärjestelmän virheenkorjauksen opiskeluun:

- [Debug.Trace-moduuli](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html) 
- [Monad-logger](https://hackage.haskell.org/package/monad-logger)
- [Fast-logger](https://hackage.haskell.org/package/fast-logger)
- [Haskell-virheenkorjauksen parhaat käytännöt](https://wiki.haskell.org/Debugging)
- [System.IO.Unsafe](https://hackage.haskell.org/package/base/docs/System-IO-Unsafe.html)

Muista, virheenkorjauksen oppiminen ja hallinta ovat tärkeitä taitoja jokaiselle ohjelmoijalle. Jatka harjoittelua ja koodaa onnellisesti!