---
title:    "Bash: Komentoriviparametrien lukeminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukijan kannattaa kiinnostua komentoriviargumenttien lukemisesta Bash-ohjelmoinnissa? Komentoriviargumentit ovat erittäin kätevä tapa käyttää ohjelmaa, sillä ne mahdollistavat käyttäjän antaa tietoa ohjelmalle ohjelman suorituksen yhteydessä. Näin ohjelma voi reagoida eri tavoin eri käyttäjän syötteisiin.

## Miten

Komentoriviargumentit luetaan Bash-skriptissä muuttujina, jotka on merkitty dollarimerkillä eli `$`. Ensimmäinen argumentti on `$1`, toinen `$2` ja niin edelleen. Esimerkiksi, jos haluamme käyttää komennon avulla käyttäjän antamaa etunimeä, voimme kirjoittaa `echo "Hei, $1!"` Bash-skriptissä. Kun käytämme tätä skriptiä komentoriviltä antamalla etunimen ensimmäisenä argumenttina, tulostus olisi esimerkiksi "Hei, Maria!".

```Bash
#!/bin/bash

echo "Hei, $1!"
```

Jos haluamme käyttää useampaa komentoriviargumenttia, voimme käyttää niitä esimerkiksi komentoriviltä annettujen lukuoperaatioiden kanssa:

```Bash
#!/bin/bash

echo "Numeroita yhteensä: $(($1 + $2 + $3))"
```

Kun suoritamme tämän skriptin komentoriviltä esimerkiksi komennolla `bash summa.sh 3 5 2`, tulostus olisi "Numeroita yhteensä: 10". Huomaa, että argumentit luetaan aina merkkijonoina, joten jos haluamme käyttää niitä lukuoperaatioissa, niitä täytyy muuttaa halutunlaisiksi (esim. 3 --> 3).

## Syventyvä oppiminen

Voimme myös käsitellä komentoriviargumentteja taulukoissa Bash-skripteissä. Tämä on hyödyllistä esimerkiksi silloin, kun haluamme käyttää tuntematonta määrää argumentteja. Voimme käyttää `"$@"` muuttujaa, joka sisältää kaikki komentoriviargumentit tavallisena taulukkona. Voimme myös käyttää `"$#"` muuttujaa, joka kertoo, kuinka monta argumenttia annettiin komentorivillä.

```Bash
#!/bin/bash

echo "Anna komentoriviargumentteja:"
for arg in "$@"
do
  echo "$arg"
done

echo "Argumenttien määrä: $#"
```

Kun suoritamme tämän skriptin komentoriviltä esimerkiksi komennolla `bash taulukko.sh Maria Janne Maija`, tulostus olisi:

Anna komentoriviargumentteja:
Maria
Janne
Maija

Argumenttien määrä: 3

## Katso myös

- [Bash-skriptit - virallinen ohje](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Komentoriviargumentit – Miikka Pakarinen](https://miikka.xyz/blog/ohjelmointi/luento1/#29.3)