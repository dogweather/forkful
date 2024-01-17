---
title:                "Tekstitiedoston kirjoittaminen."
html_title:           "Fish Shell: Tekstitiedoston kirjoittaminen."
simple_title:         "Tekstitiedoston kirjoittaminen."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Teksti tiedoston kirjoittaminen on oleellinen osa ohjelmointia. Koodausprojekteissa saattaa olla tarvetta tallentaa ja lukea erilaisia tietoja, kuten käyttäjän syöttämiä arvoja tai ohjelman tuottamia tuloksia. Tämä onnistuu helposti teksti tiedostojen avulla ja siksi ohjelmoijat käyttävät niitä usein.

## Kuinka tehdä:
Fish Shellilla teksti tiedostojen kirjoittaminen on helppoa käyttäen `echo` komentoa ja `>` operaattoria. Alla on esimerkki kirjoittamisesta uuteen tiedostoon nimeltä "teksti.txt":

```
Fish Shell: echo "Tämä on uusi teksti tiedosto!" > teksti.txt
```

Voit myös käyttää `>>` operaattoria, joka lisää tekstiä jo olemassa olevaan tiedostoon sen sijaan että kirjoittaisi uuden tiedoston. Alla on esimerkki:

```
Fish Shell: echo "Tämä on lisättya tekstiä!" >> teksti.txt
```

Voit tarkistaa tiedoston sisällön käyttämällä `cat` komentoa:

```
Fish Shell: cat teksti.txt
Tämä on uusi teksti tiedosto!
Tämä on lisättyä tekstiä!
```

## Syvempi sukellus:
Teksti tiedostojen kirjoittaminen on ollut mahdollista ohjelmoinnin alusta asti. Alun perin tietokoneet käyttivät teksti tiedostoja ohjeiden tallentamiseen ja kommunikointiin käyttäjän kanssa. Nykyään on olemassa monia muita tapoja tallentaa ja lukea tietoja, kuten tietokannoilla ja JSON tiedostoilla. Jotkut käyttävät myös tehokkaampia pyöreitä -> koodaus virheitä välttääkseen.

## Katso myös:
[Teksti tiedostojen lukeminen Fish Shellilla](https://github.com/fish-shell/fish-shell/blob/master/doc_src/faq.md#How-do-I-access-the-contents-of-a-file)
[Teksti tiedostojen käsittely ohjelmoinnissa](https://www.codecademy.com/articles/txt-extensions)
[Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/)