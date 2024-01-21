---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:44.066966-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden sovelluksen tai kirjaston koodaamisen käynnistämistä. Ohjelmoijat tekevät tämän siksi, että he voivat luoda asioiden toiminnallisuuksia alusta, ratkaista ongelmia tai opetella uusia tekniikoita.

## Kuinka:

```gleam
import gleam/io

fn main() {
  io.println("Hei! Aloita uusi Gleam-projekti näin:")
  // Asenna ensin Gleam ja käytä sitten komentoa alla
  os.cmd("gleam new minun_projektini").run()
}
```

Kun suoritat yllä olevan, näet:
```
Hei! Aloita uusi Gleam-projekti näin:
* luodaan minun_projektini/
* luodaan minun_projektini/gleam.toml
* luodaan minun_projektini/src/
* luodaan minun_projektini/src/minun_projektini.gleam
...
Valmis! Seuraavaksi, ...
```

## Syvä Sukellus:

Kun aloitat uuden projektin Gleamissa, olet periaatteessa kirjoittamassa ensimmäistä koodiviivaa puhtaalta pöydältä. Historiallisesti Gleam kehitettiin tuomaan tyyppiturvallisuutta Erlangin ekosysteemiin, joka on tiedetty sen rinnakkaisuuden hallinnasta. Kun verrataan Gleamiin, muut kielen luontiin tarkoitetut välineet kuten `cargo` Rustissa tai `npm` Node.js:ssä, toimivat samankaltaisella periaatteella - ne auttavat alustamaan projektin rungon.

Tärkeät yksityiskohtia uuden projektin alustuksessa on riippuvuuksienhallinta ja projektin rakenteen standardointi. Gleamissa `gleam.toml` tiedosto hoitaa riippuvuuksien versiot ja konfiguraation. Projektirakenteen standardointi takaa, että eri kehittäjät tietävät mistä löytää mitäkin.

## Tutustu Lisää:

- Gleam kielen virallinen opas: [https://gleam.run/book](https://gleam.run/book)
- GitHub-sivu esimerkkeineen ja ohjeineen: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Gleam-yhteisö foorumeilla ja keskustelukanavilla: [https://gleam.run/community/](https://gleam.run/community/)