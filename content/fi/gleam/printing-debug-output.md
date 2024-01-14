---
title:                "Gleam: Tulostaminen virheenkorjaustuloste"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Ihmisten luonteen mukaan me haluamme tietää, mitä tietokoneemme tekee ohjelmia suorittaessaan. Koodistamme saattaa löytyä virheitä, toimimattomia osia tai muuten vain haluamme nähdä, miksi ohjelma käyttäytyy juuri niin kuin käyttäytyy. Päästäksemme selville näistä asioista, meidän täytyy tulostaa debug-tietoa.

## Kuinka tulostaa debug-tietoa

Debug-tiedon tulostaminen on helppoa käyttäen Gleamia. Käytä yksinkertaisesti ```Gleam.debug```, ja tulosta haluamasi muuttujat tai tekstit.

```
Gleam.debug("Hei, Gleam!");
Gleam.debug(my_variable);
```

Tämä tulostaa konsoliin "Hei, Gleam!" ja muuttujan arvon.

## Syvempi sukellus

Debug-tietoa voi olla hyödyllistä käyttää myös monimutkaisempien ohjelmien kanssa. Voit tulostaa tietoa monista eri muuttujista ja nähdä, mitä tapahtuu kunkin ohjelman osan suorittaessa. Voit myös vianetsiä virheitä ja optimoida ohjelman suoritusta debug-tiedon avulla.

## Katso myös

- [Ohjeet Gleam-koodin debuggaamiseen](https://gleam.run/articles/debugging-gleam/)
- [Gleam-koodin dokumentaatio](https://gleam.run/documentation/)