---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Bash: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä joskus sinun on tarvittava kirjoittaa teksti-tiedosto ja sinusta tuntuu siltä, että haluat tehdä sen terminalilla. Tai ehkä olet vain utelias ja haluat oppia uutta taitoa. Tässä artikkelissa opit, kuinka kirjoittaa teksti-tiedosto Bashilla.

## Kuinka

Bash on ohjelmointikieli, jota käytetään yleisesti komentorivin kautta tapahtuvaan tiedostojen hallintaan ja tekstinkäsittelyyn. Kirjoittaaksesi teksti-tiedoston Bashilla, sinun täytyy ensin avata tiedostoeditori ja luoda uusi tiedosto. Tämän jälkeen voit kirjoittaa haluamasi tekstin ja tallentaa tiedoston käyttäen oikeaa komentoa.

Esimerkiksi, jos haluat luoda uuden tiedoston nimeltä "tekstitiedosto.txt", kirjoita ensin komento "touch tekstitiedosto.txt" luodaksesi uuden tyhjän tiedoston. Sen jälkeen kirjoita "nano tekstitiedosto.txt" avataksesi tiedostoneditorin ja kirjoita haluamasi teksti. Kun olet valmis, paina "control + x" tallentaaksesi muutokset ja poistuaksesi editorista.

Tässä on esimerkki kirjoitetusta teksti-tiedostosta:

```Bash
Tämä on teksti-tiedosto, joka on kirjoitettu Bashilla.
Bash on helppo tapa hallita ja käsitellä tekstitiedostoja.
Toivottavasti tämä artikkeli auttaa sinua oppimaan lisää!
```

Voit myös käyttää muita tekstieditoreita, kuten Vim tai Emacs, kirjoittaaksesi Bashilla teksti-tiedostoja. Kaikki toimivat periaatteessa samalla tavalla, mutta niissä voi olla hieman erilaisia komentoja ja toimintoja.

## Syvä sukellus

Kun kirjoitat teksti-tiedostoa Bashilla, on tärkeää muistaa käyttää oikeita pääteitä ja merkintöjä. Esimerkiksi, jos haluat lisätä uuden rivin tekstiin, sinun täytyy käyttää "\n" merkintää sen sijaan, että painaisit enter-näppäintä. Samoin, jos haluat lisätä välilyönnin, käytä "\ " merkintää sen sijaan, että painaisit välilyöntinäppäintä.

Lisäksi, Bashilla on paljon erilaisia komentoja, jotka voit käyttää teksti-tiedostoihin liittyvissä tehtävissä, kuten hakemisto- ja tiedostojen hallinnassa. Kannattaa tutustua näihin komentoihin ja niiden toimintoihin paremmin, jos haluat tehdä enemmän Bashilla teksti-tiedostojen kanssa.

## Katso myös

Tässä muutamia hyödyllisiä linkkejä oppimateriaaleihin Bashista ja teksti-tiedoston kirjoittamisesta:

- https://linuxize.com/post/bash-tutorial-for-beginners/
- https://www.tutorialspoint.com/unix_commands/echo.htm
- https://www.codecademy.com/learn/learn-the-command-line
- https://www.gnu.org/software/bash/