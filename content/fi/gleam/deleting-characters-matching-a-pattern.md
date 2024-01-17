---
title:                "Poista merkkejä, jotka vastaavat kaavaa."
html_title:           "Gleam: Poista merkkejä, jotka vastaavat kaavaa."
simple_title:         "Poista merkkejä, jotka vastaavat kaavaa."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Miksi ohjelmoijat poistavat merkkejä, jotka vastaavat tiettyä kaavaa? Yksinkertaisesti sanottuna, tämä auttaa käsittelemään merkkijonoja ja löytämään tietynlaisia merkkejä tietorakenteista. Esimerkiksi voit poistaa kaikki numerot sisältävät merkit tai poistaa ylimääräiset välilyönnit.

# Miten tehdä:

Voit käyttää `Gleam.delete_chars_matching`-funktiota poistaaksesi haluamasi merkit tietystä merkkijonosta. Katso alla olevaa esimerkkiä:

```
Gleam.delete_chars_matching("12Hello34", "[0-9]")
```

Tämä palauttaa merkkijonon "Hello".

# Syväsukellus:

## Historiallinen tausta:

Poistamisen merkkijonojen vastaavien merkkien käyttö on ollut pitkään tärkeä osa ohjelmointia. Ennen vanhaan ohjelmoijat joutuivat usein kirjoittamaan monimutkaisia koodinpätkiä poistaakseen haluamansa merkit, mutta nykyään saatavilla on erilaisia valmiita työkaluja, kuten `Gleam.delete_chars_matching`, jotka helpottavat tätä tehtävää.

## Vaihtoehtoja:

On myös muita tapoja poistaa merkkijonojen vastaavia merkkejä, kuten käyttämällä säännöllisiä lausekkeita tai iterointia. Kuitenkin `Gleam.delete_chars_matching`-funktiota käyttämällä pääset eroon monimutkaisesta koodista ja voit helposti muokata haluamiasi merkkien poistosääntöjä.

## Toteutukseen liittyvää:

`Gleam.delete_chars_matching`-funktio käyttää taustalla `String.filter`-funktiota rakentaakseen uuden merkkijonon, jossa haluamamme merkit eivät enää esiinny. Tämä takaa suorituskykyisen ja tarkan tuloksen.

# Katso myös:

Voit lukea lisää `Gleam.delete_chars_matching`-funktiosta [Gleamin virallisesta dokumentaatiosta](https://gleam.run/standard-library.html#delete_chars_matching). Voit myös tutustua `String.filter`-funktioon [täältä](https://gleam.run/standard-library.html#filter-string).