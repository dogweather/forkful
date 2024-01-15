---
title:                "Alaryhmien erottaminen"
html_title:           "Bash: Alaryhmien erottaminen"
simple_title:         "Alaryhmien erottaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi erottaa alijonon? On monia tilanteita, joissa tämä voi olla hyödyllistä, kuten tiettyjen tietojen etsiminen tai tiedostonimen muokkaaminen.

## Kuinka tehdä

Voit erottaa alijonon helposti käyttämällä Bashia. Tässä on muutama esimerkki ja niiden tuottamat tulosteet:

```Bash
# Eronnan alussa 3 merkkiä ja tulostaa jäljellä olevan stringin
echo "Tervetuloa" | cut -c 4-
>tuloa 

# Erontaa kolme viimeistä merkkiä
echo "Hei maailma" | cut -c -3
>lma 

# Erontaa välilyönnin kohdalta
echo "Omena, Appelsiini, Banaani" | cut -d " " -f 2
>Appelsiini
```

Voit myös tallentaa erottamasi osion muuttujaan käyttämällä ```$(...)```:

```Bash
# Tallentaa nimen loppuosan muuttujaan
name=$(echo "John Smith" | cut -d " " -f 2)
echo $name
>Smith
```

## Syvällinen sukellus

Bashin ```cut```-komennolla on monia vaihtoehtoja, jotka antavat sinulle suuremman tarkkuuden alijonon erottamisessa. Voit esimerkiksi erottaa vain tietyt merkit lopusta tai keskeltä käyttämällä ```-c``` vaihtoehtoa ja antamalla merkkien sijainnit. Tai voit erottaa osion tietystä erottimesta käyttämällä ```-d``` vaihtoehtoa ja antamalla erottimen. Voit lukea lisää Bashin ```cut```-komennosta [täältä](https://www.thegeekstuff.com/2013/06/cut-command-examples/).

## Katso myös

- [ExplainShell - Tietoa cut-komennosta](https://explainshell.com/)
- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/)
- [Bashin opetusohjelmat ja esimerkit](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)