---
title:                "C++: Kirjoittaminen standardivirheeseen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa virhetiedostoon?

Kirjoittaminen standardivirhetiedostoon (stderr) on tärkeä osa C++ ohjelmointia. Se antaa kehittäjille mahdollisuuden tarkkailla ja hallita virheitä, jotka voivat ilmetä ohjelman suorituksen aikana. Tämä on erityisen hyödyllistä silloin, kun halutaan varmistaa, että ohjelma toimii odotetulla tavalla.

## Kuinka tehdä se?

Kirjoittaminen stderr:iin on melko yksinkertaista C++:ssa. Seuraavassa esimerkissä näytämme, kuinka voit tulostaa virheilmoituksen virhetiedostoon käyttämällä standardikirjaston funktiota `fprintf()`:

```C++
#include <stdio.h>

int main() {
  fprintf(stderr, "Virheilmoitus: Luvun jakaminen nollalla ei ole sallittua\n");
  return 0;
}
```

**Tulostus:**

```
Virheilmoitus: Luvun jakaminen nollalla ei ole sallittua
```

## Syvällisempi sukellus

Voit myös käyttää `cerr` ja `clog` objekteja kirjoittamaan virheilmoituksia ja lokitietoja stderr:iin. Ne ovat osa C++ standardikirjastoa ja mahdollistavat virheiden ja muun merkittävän tiedon erottamisen lokitietojen avulla.

Suuremmissa ohjelmissa voi olla hyödyllistä luoda oma virhetiedostoluokka, joka käsittelee eri virhetilanteita ja kirjoittaa ne stderr:iin. Tämä auttaa eriyttämään koodia ja helpottaa virheiden ja lokitietojen hallintaa.

## Katso myös

- [C++ Standard Library - virhetiedosto](http://www.cplusplus.com/reference/cstdio/stderr/)
- [Cppreference - C - virheiden käsittely](https://en.cppreference.com/w/c/error)

Voit oppia lisää C++:n virheiden käsittelystä ja stderr:iin kirjoittamisesta näiden linkkien kautta. Toivottavasti tämä artikkeli auttaa sinua ymmärtämään, miksi ja kuinka stderr:iin kirjoittaminen on tärkeää ohjelmoinnissa. Onnea ohjelmointiin!