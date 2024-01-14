---
title:    "Bash: Satunnaislukujen luominen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi: Satunnaislukujen generoiminen

Satunnaislukujen generoiminen on tärkeä osa monia ohjelmointiprojekteja, kuten pelin kehittämistä tai datan analysointia. Satunnaislukujen avulla voidaan simuloida tiettyjä tilanteita ja testata ohjelmien toimivuutta erilaisilla syötteillä.

## Kuinka: Esimerkkejä koodista ja tulosteista

Satunnaislukujen generoiminen on helppoa Bash-ohjelmoinnissa käyttäen ```$RANDOM``` -muuttujaa. Tämä muuttuja sisältää satunnaisen numeron välillä 0 ja 32767. Alla olevassa koodiesimerkissä generoidaan 10 satunnaista kokonaislukua ja tulostetaan ne ruudulle.

```Bash
for i in {1..10}
do
  echo $RANDOM
done
```

Tuloste:

```Bash
16096
26525
10594
28066
16216
1281
22208
24028
23583
3361
```

Käyttäjä voi myös itse määrittää haluamansa alku- ja loppuluvun käyttämällä ```$RANDOM``` -muuttujaa seuraavassa muodossa: ```$((min + $RANDOM % (max-min+1)))```. Alla olevassa esimerkissä käyttäjältä pyydetään syöttämään haluamansa väliluku, jonka jälkeen tulostetaan 5 satunnaista lukua tältä väliltä.

```Bash
echo "Anna alku- ja loppuluku:"
read min max

for i in {1..5}
do
  echo $(($min + $RANDOM % ($max-$min+1)))
done
```

Esimerkkituloste:

```Bash
Anna alku- ja loppuluku:
10 20
14
19
13
10
18
```

## Syväsukellus: Lisätietoa satunnaislukujen generoimisesta

Vaikka ```$RANDOM``` on kätevä tapa generoida satunnaisia lukuja Bash-ohjelmoinnissa, on hyvä ottaa huomioon muutamia seikkoja sen käytössä. Ensinnäkin, ```$RANDOM``` ei välttämättä ole täysin satunnainen, vaan sen sisäinen mekanismi kyseisen luvun generoimiseen perustuu tiettyyn algoritmiin. Tämä voi johtaa siihen, että samassa ohjelman suorituksessa generoidut luvut saattavat toistaa itseään.

Toisekseen, ```$RANDOM``` -muuttuja on käyttäjäkohtainen, eli jokaisella käyttäjällä on oma satunnaislukunsa. Tämä tarkoittaa sitä, että jos useampi käyttäjä suorittaa samaa Bash-skriptiä samanaikaisesti, lukuja ei voida taata täysin satunnaisiksi.

On myös mahdollista käyttää muita tapoja generoida satunnaislukuja Bash-ohjelmoinnissa, kuten käyttämällä OpenSSL-komentoa tai ulkoista satunnaislukugeneraattoria.

## Katso myös:

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/)
- [Bash Satunnaisluku Generaattori - GitHub-repositorio](https://github.com/robertsertic/bash-random)
- [OpenSSL - virallinen dokumentaatio](https://www.openssl.org/docs/manmaster/man1/openssl.html)
- [Random.org - ulkoinen satunnaislukugeneraattori](https://www.random.org/)