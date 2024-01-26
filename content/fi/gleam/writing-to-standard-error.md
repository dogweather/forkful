---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kirjoittaminen standardivirheeseen ('standard error') tarkoittaa virheviestien ja logien ohjaamista erilliseen tulostevirtaan ohjelmassa, mikä auttaa erottamaan ne tavallisesta tulosteesta. Ohjelmoijat käyttävät tätä erottelemaan sovelluksen normaalin toiminnan ja virhetilanteiden käsittelyn, mikä helpottaa debuggausta ja logien analysointia.

## Näin teet:
Gleamissa standardivirheeseen kirjoittaminen voidaan tehdä käyttäen `io`-moduulia. Tässä yksinkertainen esimerkki ja sen tuloste:

```gleam
import gleam/io

pub fn main() {
  io.print("Tämä menee standarditulosteeseen.\n")
  io.eprint("Tämä menee standardivirheeseen.\n")
}
```

Jos suoritat tämän koodin, saat:
```
Tämä menee standarditulosteeseen.
Tämä menee standardivirheeseen.
```

Huomaa, kuinka molemmat viestit tulostuvat, mutta ne voidaan ohjata eri kohteisiin käyttöjärjestelmän tasolla.

## Syväsukellus:
Historiallisesti standardituloste (stdout) ja standardivirhe (stderr) eriytettiin, jotta normaali output ja virheilmoitukset voitaisiin käsitellä eri tavoin. Unix-järjestelmissä tämä on yleinen tapa ja se on vakiintunut käytäntö monissa ohjelmointikielissä. Vaihtoehtoina voisi olla lokitiedostojen kirjoittaminen tai verkkopalveluihin lähettäminen, mutta standardivirhe on nopea ja yksinkertainen ratkaisu suoraan terminalille raportointiin. Gleamissa toteutus perustuu Erlangin virtuaalikoneeseen: standardituloste ja -virhe ovat prosessin metatietoja, jotka ohjaavat tulostetta oikeisiin paikkoihin.

## Katso myös:
- Unixin standarditulosteesta ja -virheestä: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)
- Ohjelman logituksen parhaista käytännöistä: [https://12factor.net/logs](https://12factor.net/logs)
