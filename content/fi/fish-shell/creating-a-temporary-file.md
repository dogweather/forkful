---
title:    "Fish Shell: Väliaikaisen tiedoston luominen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Luoden tilapäinen tiedosto voi olla hyödyllistä monissa Fish Shell ohjelmissa, kuten tallentaessa tiedostoja tai käsitellessä tiedostojen manipulointia. Se voi myös auttaa pitämään koodin selkeämpänä ja turvallisempana.

## Kuinka

Ohjelmointiesimerkki käyttäen tilapäistä tiedostoa ```Fish Shell``` koodilohkossa:

```
set temp (mktemp)
echo "Tämä on tilapäinen tiedosto" > $temp
cat $temp
```

Output: Tämä on tilapäinen tiedosto

Tässä esimerkissä käytetään ```mktemp``` komentoa luomaan tilapäinen tiedosto ja sitten kirjoitetaan tekstiä tiedostoon ```echo``` komennolla. Lopuksi tiedoston sisältö tulostetaan käyttäen ```cat``` komentoa. Kun komento suoritetaan, käyttäjä näkee tuloksen ilman, että tarvitsee luoda pysyvää tiedostoa, mikä voi olla hyödyllistä tilanteissa, joissa halutaan käsitellä tietoa väliaikaisesti.

## Syväsukellus

Fish Shell tarjoaa useita vaihtoehtoja luoda tilapäisiä tiedostoja. ```mktemp``` komento luo tiedoston, jonka nimi on automaattisesti generoitu ja uniikki. Tämä tekee siitä turvallisemman vaihtoehdon kuin luoda tiedosto itsellesi, sillä se minimoi mahdollisuuden luoda jo olemassaolevan tiedoston.

Toinen vaihtoehto on käyttää ```set temp (exec mktemp)```komentoa, mikä luo uuden tilapäisen tiedoston ja asettaa sen muuttujaan.

## Katso myös

- [Fish Shell virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Määrittele muuttujat Fish Shell:ssa](https://webkurssi.net/ohjelmointi/shell/muuttujat)
- [Komennot ja ohjelmointi Fish Shell:ssa](https://www.shellscript.sh/index.html)