---
title:    "Bash: Kirjoittaminen standardivirheeseen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi kirjoittaa virheilmoituksia standardi virhevirtaan? Tässä blogikirjoituksessa käsittelemme tätä kysymystä ja kerromme, miten tämä voi olla hyödyllistä Bash-ohjelmoijille.

## Miten

Kirjoittaminen standardi virhevirtaan on hyvä tapa tulostaa virheilmoituksia Bash-skripteissä. Voit tehdä tämän käyttämällä ">&2" merkkiä komennolle, joka tulostaa halutun virheviestin. Tässä on esimerkki koodista:

```Bash 
#!/bin/bash

# Etsitään tiedostoa nimeltä "kuvat.jpg"
grep "kuvat.jpg" /home/käyttäjä/kansio >&2

# Jos tiedostoa ei löydy, tulostetaan virheilmoitus
if [ $? != 0 ]; then
    echo "Tiedostoa ei löydy!" >&2
fi
```

Ja tässä on esimerkiksi, miten output näyttäisi, jos tiedostoa ei löydy:

```
Tiedostoa ei löydy!
```

## Syväsukellus

Kirjoittaminen standardi virhevirtaan auttaa sinua erottamaan virheilmoitukset tavallisesta tulosteesta. Tämä on hyödyllistä, kun haluat ohjelman käyttäjien huomaavan ja korjaavan virheet.

Standardi virhevirta (STDERR) on yksi kolmesta oletusvirtauksesta Bash-skripteissä. Kokonaisuudessaan Bash tukee kolmea oletusvirtausta: standardi sisääntulo (STDIN), standardi tuloste (STDOUT) ja standardi virhevirta (STDERR). Käyttämällä ">&2" merkkiä, voit ohjata haluamasi tulosteen standardi virhevirtaan.

Yksi tärkeä asia, joka kannattaa muistaa, on se, että STDIN, STDOUT ja STDERR voidaan ohjata eri paikkoihin. Näin voit esimerkiksi ohjata STDINin tuloputkeen ja STDOUTin ja STDERRin tiedostoihin.

## Katso myös

- [BASH Beginner's Guide - File Descriptors](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_04.html)
- [BASH Guide for Beginners - I/O redirection](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [BASH Scripting Tutorial - Standard I/O and redirection](https://ryanstutorials.net/bash-scripting-tutorial/bash-i-o.php#redirect)

Kiitos kun luit tämän blogikirjoituksen! Toivottavasti siitä oli hyötyä Bash-ohjelmoijille. Muista aina tarkistaa virheilmoitukset standardi virhevirrasta, jotta voit parantaa ohjelmasi toimivuutta ja luotettavuutta. 

Nähdään seuraavassa kirjoituksessa!