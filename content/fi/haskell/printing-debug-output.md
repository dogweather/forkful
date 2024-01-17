---
title:                "Poikkeustulosteiden tulostus"
html_title:           "Haskell: Poikkeustulosteiden tulostus"
simple_title:         "Poikkeustulosteiden tulostus"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Painattaminen debug-tulosteilla on yksinkertainen mutta tärkeä tapa tarkistaa ja korjata virheitä ohjelmoinnissa. Kun painatamme koodiamme, näemme tarkalleen missä vaiheessa ohjelma toimii ja mikä mahdollisesti aiheuttaa virheen. Näin voimme nopeasti etsiä ja korjata ongelmat.

## Tee näin:

```Haskell
-- Esimerkki lista-arvojen tulostamisesta
lista = [1, 2, 3, 4, 5]
putStrLn "Lista:"
print lista
```

Tämä koodi tulostaa "Lista:" ja [1, 2, 3, 4, 5] näytölle. Voit myös käyttää ```show```-funktiota, joka muuntaa arvon merkkijonoksi ja tulostaa sen.

```Haskell
-- Esimerkki muuttujan tulostamisesta
nimi = "Johanna"
putStrLn "Hei, " ++ nimi ++ "!"
```

## Syvempi sukellus:

Debug-tulosteiden käyttö ei ole mikään uusi keksintö. Ohjelmistojen kehittäjät ovat käyttäneet sitä jo vuosia virheiden paikantamiseen ja korjaamiseen. On myös muita tapoja tarkkailla koodia, kuten yksikkötestauksen avulla, mutta debug-tulosteen käyttö on edelleen nopea ja yksinkertainen tapa tarkistaa koodia.

Voit myös käyttää erilaisia debug-tulosteiden muotoiluvaihtoehtoja, kuten värillisiä viestejä, jotta ne erottuvat paremmin koodista. Voit lukea lisää näistä vaihtoehdoista [täältä](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html).

## Katso myös:

- [Yksikkötestaus Haskellissa](https://haskell.org/haskellwiki/Unit_testing)
- [Juuriuksien käyttö Haskellissa](https://haskell.org/haskellwiki/Using_Channel_Ids)
- [Haskellin virallinen dokumentaatio debug-tulosteiden käytöstä](https://wiki.haskell.org/Debugging)