---
title:    "Bash: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Yksi yleinen tarve Bash-ohjelmoinnissa on muuntaa merkkijono pieniksi kirjaimiksi (lower case). Tämä voi olla hyödyllistä esimerkiksi silloin, kun ohjelmassa käsitellään käyttäjän syötteitä ja halutaan varmistaa, että ne ovat yhdenmukaisessa muodossa.

## Kuinka tehdä

Bashilla merkkijonon muuntaminen pieniksi kirjaimiksi on helppoa ja nopeaa. Käytämme tähän tarkoitukseen merkkijonofunktiota "tr", joka korvaa merkkijonon kaikki kirjaimet toisella annetulla merkkijonolla. Alla on esimerkki siitä, kuinka muuntaa merkkijono "Terve" pieniksi kirjaimiksi:

```Bash
echo "Terve" | tr '[:upper:]' '[:lower:]'
```
Tämän komennon tuloste on "terve". Kuten näet, "tr" funktiota voidaan käyttää yhdessä putkella (|) ja echo-komennon kanssa.

Voit myös käyttää muuttujia tässä prosessissa. Alla olevassa esimerkissä tallennamme ensin "Terve" muuttujaan ja sitten muunnamme sen pieniksi kirjaimiksi ja tulostamme sen näytölle:

```Bash
word="Terve"
echo $word | tr '[:upper:]' '[:lower:]'
```
Tuloste on jälleen "terve".

## Syvällinen sukellus

Tr-funktion lisäksi Bashilla on myös muita tapoja muuntaa merkkijono pieniksi kirjaimiksi. Yksi niistä on käyttää "$" (parameter expansion) merkintää. Tämä merkintä korvaa annetun muuttujan tai arvon pieniksi kirjaimiksi. Alla on esimerkki tästä:

```Bash
word="Terve"
echo ${word,,}
```
Tämä tulostaa jälleen "terve". Myös tässä tapauksessa voit käyttää muita komentoja, kuten "read", "readarray" tai "echo", muuttujan sijaan.

On myös tärkeää muistaa, että Bashin käyttämän oletusmerkistön mukaan "tr" ei muunna ääkkösiä ja muita erikoismerkkejä oikein. Jos haluat varmistaa, että kaikki merkit muunnetaan oikein, voit käyttää "sed" komentoa yhdessä "tr" kanssa. Esimerkiksi:

```Bash
echo "Tërvë" | sed 's/.*/\L&/' | tr '[:upper:]' '[:lower:]'
```
Tässä tapauksessa sekä "sed" että "tr" ovat tarpeen, jotta kaikki kirjaimet muuntuisivat oikein.

## Katso myös

- Informatiivinen artikkeli Bashin muuttujista ja niiden käyttämisestä: https://www.linux.com/blog/learn/linux-skills/understanding-bash-shell-variables/
- Opas Bash-ohjelmoinnin perusteista: https://www.bash.academy/
- Bashin viralliset dokumentaatiot (englanniksi): https://www.gnu.org/software/bash/