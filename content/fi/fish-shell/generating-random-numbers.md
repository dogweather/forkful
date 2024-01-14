---
title:    "Fish Shell: Sattumanvaraisten lukujen luominen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa random-numeroiden luominen voi olla hyödyllistä. Se voi esimerkiksi auttaa sinua testaamaan koodiasi tai luomaan satunnaisia parametreja simulaatioihin. Fish Shellin sisäänrakennettu random-toiminto tekee tästä entistä helpompaa.

## Miten tehdä

Käyttämällä Fish Shellin `random`-funktiota voit luoda satunnaisia numeroita haluamallasi välillä. Esimerkiksi, jos haluat satunnaisen luvun väliltä 1-10, voit käyttää seuraavaa komentoa:

```Fish Shell
random 1 10
```
Tämä tulostaa random-numeron väliltä 1-10, esimerkiksi "5". Voit myös asettaa haluamasi luvun alkuun, jolloin numerot ovat väliltä 0-numeroon asti. Esimerkiksi, jos haluat satunnaisen numeron väliltä 20-50, käytä seuraavaa komentoa:

```Fish Shell
random 20 50
```
Tämä tulostaa random-numeron väliltä 20-50, esimerkiksi "38". Voit myös rajata tuloksen tiettyyn tarkkuuteen käyttämällä `-n` -valitsinta. Esimerkiksi, jos haluat random-numeroiden olevan tasan kaksi desimaalia, käytä komentoa:

```Fish Shell
random -n 2 1 10
```
Tämä tulostaa random-numeron väliltä 1-10, esimerkiksi "2.55". Voit käyttää näitä komentoja myös scripteissä ja soveltaa niitä eri tavoin tarpeen mukaan.

## Syvällinen sukellus

Fish Shell käyttää sisäistä /dev/urandom -laitetta random-numeroiden luomisessa. Tämä laite tuottaa lukuja käyttäen "entropy gathering" järjestelmää, joka kerää satunnaisia muuttujia ympäristöstä, kuten käynnissä olevan prosessin PID:n, lähettäjän IP-osoitteen tai lausekkeisiin kuluvia nanosekunteja. Näiden muuttujien yhdistelmä tuottaa lopulliset random-numerot.

## Katso myös

- [Fish Shellin dokumentaatio random-funktiosta](https://fishshell.com/docs/current/cmds/random.html)
- [/dev/urandomin selitys](https://en.wikipedia.org/wiki//dev/random)