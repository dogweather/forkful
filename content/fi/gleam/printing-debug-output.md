---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:59.830966-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Debug-tulostus auttaa näkemään mitä ohjelmassasi tapahtuu – se on kuin vilkaisisi konepellin alle ajaessasi. Ohjelmoijat käyttävät sitä virheiden löytämiseen ja korjaamiseen sekä ohjelman toiminnan ymmärtämiseen paremmin.

## How to: - Kuinka:
```gleam
// Gleam-esimerkki debug-tulosteille
fn main() {
  let secret_number = 42
  dbg!("Salainen numero on: ", secret_number)
  // Tekemistäsi täällä...
}

// Tulostettava esimerkkiteksti:
// > Salainen numero on: 42
```

```gleam
// Toinen tapa: käytä `io.debug` -funktiota
import gleam/io

fn main() {
  let message = "Tähän jotakin tärkeää"
  io.debug(message)
  // Jatka koodaamista...
}

// Tulostuu:
// Tähän jotakin tärkeää
```

## Deep Dive - Syväsukellus:
Tulostaminen on vanha keino seurata ohjelman kulkua. Ennen IDE:iden ja debuggerien aikaa, tulostus oli usein ainut tapa ymmärtää, mitä ohjelmassa tapahtuu. Gleamissa, kuten monissa muissa nykyaikaisissa kielissä, voit käyttää `dbg!`-makroa tai `io.debug`-toimintoa tulostukseen, mutta on hyvä muistaa poistaa debug-tulosteet ennen ohjelman julkaisua, jotteivät ne häiritse lopputuotetta.

Vaihtoehtoina on monia debugger-työkaluja tai vaikkapa “logitus” (logging), joka tallentaa tietoja tiedostoon myöhemmää tutkiskelua varten. Gleamissa on omat kirjastonsa sekä logitukseen että debuggaamiseen, mutta debug-tulosteet toimivat hyvänä nopeana työkaluna, kun tarvitsee vain pikaisesti tarkastaa muuttujan arvon.

## See Also - Katso Myös:
- Gleamin virallinen dokumentaatio `io`: https://gleam.run/std-lib/gleam/io/
- Debuggaus vs. logitus selitys: https://randycoulman.com/blog/2016/03/15/debugging-vs-logging/
- Artikkeli ohjelman virheenkorjauksen historiasta: https://en.wikipedia.org/wiki/Debugging#History

Muista, debug-tulostus on työkalu, ei lopullinen ratkaisu. Käytä sitä viisaasti!