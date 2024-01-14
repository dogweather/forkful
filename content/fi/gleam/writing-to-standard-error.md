---
title:                "Gleam: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi

Miksi joku haluaisi kirjoittaa standardi virheeseen (standard error)?

Vastaus on yksinkertainen - standardi virhe on tärkeä osa koodin suorittamista ja virheilmoitukset on tärkeää havaita mahdollisimman nopeasti. Kirjoittamalla standardi virheeseen, voit nähdä mahdolliset virheet ja havaita mahdolliset ongelmat koodissasi, mikä auttaa sinua korjaamaan ne nopeasti.

# Miten

Standardi virheeseen kirjoittaminen Gleam-ohjelmointikielessä on yksinkertaista. Voit käyttää standardi biblioteekin funktiota ```io:error/1``` ja antaa sille parametriksi merkkijonon, joka sisältää haluamasi virheilmoituksen. Tämä funktio kirjoittaa merkkijonon standardi virheeseen ja aiheuttaa virheen ohjelman suorituksessa. Alla on esimerkki koodista ja sen tulosteesta:

```Gleam
let virhe = "Tässä on virheilmoitus!"
let _ = io:error(virhe)
```

Tuloste:

```
Tässä on virheilmoitus!
```

# Syväluotaus

Vaikka standardi virheeseen kirjoittaminen voi tuntua yksinkertaiselta, se on tärkeä osa koodin suunnittelua ja debuggausta. On tärkeää, että käytät hyviä käytäntöjä ja huolellisesti harkitset mitä ja milloin kirjoitat standardi virheeseen. Huolellinen suunnittelu voi auttaa sinua välttämään tarpeettomia virheitä ja selkiyttää koodisi rakennetta.

# Katso myös

- [Gleam standard bibliteekki](https://gleam.run/stdlib/io.html#io:error/1)
- [Standardi virheen merkitys ohjelmoinnissa](https://www.lifewire.com/standard-error-definition-4587090)
- [Vinkkejä virheilmoitusten käsittelyyn](https://www.codinghorror.com/blog/2009/05/should-error-messages-be-humorous.html)