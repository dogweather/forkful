---
title:    "Fish Shell: Samaa kaavaa vastaavien merkkien poistaminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Jotkut saattavat ihmetellä, miksi haluaisi poistaa merkkejä, jotka vastaavat tietyssä kuviossa. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat siistiä tai muokata tiettyä merkkijonoa.

## Kuinka

Fish Shell tarjoaa kätevän tavan poistaa merkkejä, jotka vastaavat tiettyä kuvioa. Voit tehdä tämän käyttämällä `string match`-toimintoa ja lisäämällä `-d`-optio välilevynä c`haracter`.

Seuraavassa esimerkissä näet, kuinka poistat kaikki välilyönnit merkkijonosta käyttämällä `-d`-optiota:

```
Fish Shell koodiblokki:

string match -d " " "Tämä on esimerkkilause."
```

Tämän tuloksena tulostuu:

```
Tämäonesimerkkilause.
```

Voit myös poistaa useampia merkkejä käyttämällä `-d`-optiota toistuvasti, esimerkiksi:

```
Fish Shell koodiblokki:

string match -d "a" -d "e" "Tämä on esimerkkilause."
```

Tulostaa:

```
Tm on smrkkilus.
```

## Syvällinen Tutki

Voit halutessasi tarkastella tarkemmin, mitä `string match`-toiminto tekee. Voit tehdä tämän käyttämällä `-v`-optiota, joka näyttää poistettavien merkkien määrän sekä alkuperäisen ja muokatun merkkijonon. Esimerkiksi:

```
Fish Shell koodiblokki:

string match -d "a" -d "e" -v "Tämä on esimerkkilause."
```

Tulostaa:

```
String 'Tm on smrkkilus' has 2 deleted characters
Original string: Tämä on esimerkkilause.
Modified string: Tm on smrkkilus.
```

Tämä voi olla hyödyllistä, jos haluat varmistaa, että olet poistanut haluamasi merkit.

## Katso myös

* [Fish Shellin dokumentaatio poistamisesta merkkijonoja käyttäen](https://fishshell.com/docs/current/cmds/string.html#string-match)
* [Yleisiä kirjastoja ja toimintoja Fish Shellille](https://fishshell.com/docs/current/index.html#appendix-builtin-packages)