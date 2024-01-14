---
title:                "Bash: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lukea komentorivin argumentteja? Komentorivi on voimakas työkalu, joka voi auttaa sinua automatisoimaan tehtäviä ja tehdä suuria määriä työtä lyhyessä ajassa. Lukemalla komentorivin argumentteja, voit tehdä skriptejä ja ohjelmia entistä joustavammiksi ja tehokkaammiksi. Joten, jos olet kiinnostunut parantamaan ohjelmointitaitojasi ja nopeuttamaan päivittäistä työtäsi, tämä artikkeli on sinulle!

## Miten

Luultavasti olet jo käyttänyt komentoriviä ja kirjoittanut joitakin yksinkertaisia komentoja, kuten "ls" tai "cd". Mutta kuinka voit lukea ja hyödyntää käyttäjän antamia komentorivin argumentteja? Se on helppoa! Tässä on esimerkki:

```Bash
#!/bin/bash
# Tässä skriptissä hyödynnetään komentorivin argumentteja.
# Käyttäjä syöttää nimensä ja iän komentoriville.

echo "Tervetuloa, $1! Olet $2 vuotta vanha."
```

Skriptissä käytetään "echo" komentoa, jonka avulla tulostetaan käyttäjän antamat argumentit. $1 edustaa ensimmäistä argumenttia eli käyttäjän nimeä ja $2 toista argumenttia eli ikää. 

Joten, jos suoritat tämän skriptin seuraavalla tavalla:

```Bash
bash tervehdys.sh Johanna 25
```

Saat seuraavan tulosteen:

```Bash
Tervetuloa, Johanna! Olet 25 vuotta vanha.
```

Nyt voit jo arvata, kuinka voit lukea minkä tahansa määrän komentorivin argumentteja ja käyttää niitä skriptisi sisällä. Kaikenlaiset manipuloinnit ja tietojen käsittely voidaan tehdä komentorivin argumenttien avulla.

## Syvemmälle

Haluatko tietää enemmän komentorivin argumenteista ja niiden käytöstä? Voit esimerkiksi käyttää "getopts" komentoa, jolla voit antaa parametrejä komentoriville ja valita niistä tarvittavat. Voit myös käyttää "shift" komentoa siirtääksesi argumentteja ja tarkistaa niitä eri tavalla. On monia muita käyttökelpoisia temppuja ja kikkoja, joita voit oppia lukemalla lisää aiheesta tai kokeilemalla itse.

## Katso myös

Lue lisää Bash-ohjelmoinnista ja komentorivin käytöstä näistä linkeistä:

- [Bash-sivusto](https://www.gnu.org/software/bash/)
- [Bash-kirja](http://www.tldp.org/LDP/abs/html/)
- [Komentorivin argumentit Linuxissa](https://www.linux.com/training-tutorials/arguments-and-variables-bash/)