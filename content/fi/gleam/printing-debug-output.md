---
title:                "Tulostamisen virheenkorjaustulos (Virheenkorjaustulostus)"
html_title:           "Gleam: Tulostamisen virheenkorjaustulos (Virheenkorjaustulostus)"
simple_title:         "Tulostamisen virheenkorjaustulos (Virheenkorjaustulostus)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi tulostamme vianetsintätulosteita?

Vianetsintätulosteiden tulostaminen auttaa sinua näkemään, mitä koodisi tekee ja löytämään mahdollisia virheitä. Se on tärkeä työkalu ohjelmoinnissa ja auttaa sinua kehittämään parempia ohjelmia.

## Kuinka

```Gleam
// Tässä koodiesimerkissä tulostamme tekstin "Hei maailma!" vianetsintätulosteena.
import gleam/io
io.debug("Hei maailma!")
```

Koodiesimerkissä käytämme `gleam/io`-moduulia, joka tarjoaa meille `debug`-funktion vianetsintätulosteiden tulostamiseen. Voit tulostaa minkä tahansa arvon tai muuttujan käyttämällä tätä funktiota, joten voit räätälöidä tulosteet tarpeidesi mukaan.

Vianetsintätulosteet tulostuvat virran tarkkailijan konsoliin tai lokiin, joten varmista, että käytät sitä ohjelmaasi ajaessasi.

## Syvempi sukellus

Vianetsintätulosteiden tulostaminen Gleamissa on helppoa, mutta on myös muutamia huomioon otettavia asioita. Ensinnäkin, on hyvä tapa käyttää `debug`-funktion sijaan `debug_i`-funktiota, joka hyväksyy formaattiargumentteja. Tämä auttaa sinua tulostamaan monimutkaisempia arvoja selkeämmin.

Lisäksi, jos aiot tulostaa useita vianetsintätulosteita, on parempi luoda `debug`-funktio moduulin yläpuolelle, jotta sitä voidaan käyttää muissa moduuleissa. Tällä tavoin voit helposti muuttaa vianetsintätulosteen käyttöä koko ohjelmassa vain muuttamalla yhtä funktiota.

## Katso myös

- [Gleam dokumentaatio](https://gleam.run/)
- [Vianetsinnän perusteet Gleamissa](https://gleam.run/articles/debugging-intro)
- [Gleam virran tarkkailija](https://gleam.run/articles/debugging-tracing)