---
title:                "Tiedostosta lukeminen"
html_title:           "Bash: Tiedostosta lukeminen"
simple_title:         "Tiedostosta lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstikansion lukeminen on yksinkertaisesti tiedoston sisällön lukemista ohjelmakoodilla. Ohjelmoijat tekevät tätä usein saadakseen tietoa tiedostoista, jotka sisältävät tarvittavia tietoja heidän sovelluksilleen.

## Miten tehdä?

Käyttäessäsi Bash-komentoriviä, voit avata tekstikansion komennolla ```cat tiedostonimi```. Tämä tulostaa kansion sisällön komentoriville. Voit myös määrittää tulostettavan tiedoston ja käyttää sitä myöhemmin, esimerkiksi ```tulostettava_tiedosto="tiedosto.txt"``` ja ```cat $tulostettava_tiedosto``` joka tulostaa tiedoston sisällön.

## Syväsukellus

Tiedoston lukemisen historiallinen tausta liittyy tietokoneiden alkuaikoihin, jolloin tulostaminen paperille oli tärkeää. Nykyään monet muut vaihtoehdot ovat saatavilla, kuten tiedostojen lukeminen muilla ohjelmointikielillä tai eri ohjelmilla. Bashin tekstikansion lukeminen on kuitenkin edelleen suosittu tapa tiedostojen sisällön hankkimiseksi.

## Katso myös

Jos olet kiinnostunut oppimaan lisää Bash-komentorivin käytöstä, voit tarkastella [Bashin käyttöopasta](https://www.tutorialspoint.com/unix_commands/bash.htm). Lisätietoja tiedostojen lukemisesta ja manipuloinnista Bashilla löytyy myös [täältä](https://www.tecmint.com/13-basic-cat-command-examples-in-linux/).