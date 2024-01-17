---
title:                "Pakistanissa: Merkkijonon suurien kirjainten käyttö"
html_title:           "Fish Shell: Pakistanissa: Merkkijonon suurien kirjainten käyttö"
simple_title:         "Pakistanissa: Merkkijonon suurien kirjainten käyttö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Pienelläkin ohjelmointikokemuksella on varmasti tullut vastaan tehtäviä, joissa täytyy muokata tekstiä ennen sen tulostamista. Tätä varten on olemassa koodifunktioita, jotka tekevät tekstien muokkauksesta helpompaa. Yksi tällainen on "capitalize" eli suuriksi kirjaimiksi muuttaminen.

Miksi ohjelmoijat tekevät tämän? Yleensä siksi, että halutaan varmistaa, että teksti on selvästi luettavissa, esimerkiksi otsikoissa tai merkittävissä sanoissa.

## Miten:

Fish Shellissa capitalizen käyttäminen on hyvin helppoa. Tässä muutama esimerkki:

```
fish
set name "jane doe"
capitalize $name  #palauttaa "Jane Doe"
capitalize "hello world"  #palauttaa "Hello world"
```

Näitä funktioita voi myös ketjuttaa, jolloin voit yhdistää useamman tekstinmuokkausfunktion yhteen komentoon:

```
capitalize firstLetterLowercase "jane doe"  #palauttaa "Jane doe"
```

## Syvemmälle:

Capitalizen historia ulottuu aina ensimmäisiin koodikieliin, joissa on ollut mahdollisuus käsitellä tekstiä. Nykyään on myös muita tapoja muokata tekstiä, kuten "uppercasing", joka muuttaa kaikki kirjaimet suuriksi, tai "camel casing", jossa sanat kirjoitetaan yhteen ja jokainen uusi sana alkaa isolla kirjaimella.

Fish Shellin capitalizen toiminta perustuu GNU-kanto-ohjelmaan "cut", joka löytyy lähes kaikista Unix-pohjaisista käyttöjärjestelmistä. Tämän avulla capitalizen toiminta on nopeaa ja tehokasta.

## Katso myös:

- [Fish Shellin viralliset ohjeet](https://fishshell.com/docs/current/index.html)
- [GNU cutin dokumentaatio](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)