---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Bash: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

On useita tilanteita, joissa tarvitaan satunnaisten numeroiden generoimista Bash-kielellä. Tämä voi liittyä esimerkiksi salausavainten tai salasanoiden generoimiseen tai satunnaisten testitietojen luomiseen ohjelmointiin liittyvissä tehtävissä.

## Kuinka tehdä

Bash-kielessä on mahdollista generoida satunnaisia numeroita käyttämällä ```$RANDOM```-muuttujaa. Tämän muuttujan arvo muuttuu jokaisella suorituskerralla ja sen arvot vaihtelevat välillä 0-32767. Alla on esimerkkejä eri tapoista käyttää tätä muuttujaa:

- Generoi yksittäinen satunnainen numero:

```Bash
echo $((RANDOM))
```

- Generoi satunnainen luku väliltä 1-10:

```Bash
echo $((RANDOM%10+1))
```

- Generoi 10 satunnaista numeroa välillä 1-100:

```Bash
for number in {1..10}
do
  echo $((RANDOM%100+1))
done
```

Lopputuloksena saat siis 10 satunnaista lukua, esimerkiksi:

```Bash
87
34
12
67
99
23
56
45
76
1
```

## Syvemmälle sukeltaminen

Satunnaiset numerot Bash-kielessä perustuvat ```$RANDOM```-muuttujan arvoon, joka on todellisuudessa pseudosatunnainen generaattori. Tämä tarkoittaa, että vaikka numeraalit muuttuvat jokaisella suorituskerralla, niiden järjestys ja jakautuminen eivät ole täysin sattumanvaraisia. Jos tarvitset todella satunnaisia numeroita, kannattaa harkita muiden työkalujen, kuten OpenSSL:n tai GPG:n, käyttämistä.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Bashin opetusohjelmat](https://linuxconfig.org/bash-scripting-tutorial)