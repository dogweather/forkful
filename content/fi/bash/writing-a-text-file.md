---
title:                "Bash: Tiedoston kirjoittaminen"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa Bash-koodia?

Bash on komentokehotteen ohjelmointikieli, jota käytetään usein Linux- ja Unix-järjestelmissä. Kirjoittamalla tekstitiedostoja Bashilla voit automatisoida tietokoneesi toimintoja ja tehdä monimutkaisistakin tehtävistä yksinkertaisempia. Tämä säästää aikaa ja vaivaa!

## Miten kirjoittaa tekstitiedostoja Bashilla?

Bash-ohjelmointikielen kirjoittaminen on helppoa. Aloita kirjoittamalla "```Bash" ja jatka sen jälkeen koodin rivillä. Lopeta koodinpätkä kirjoittamalla "```". Esimerkiksi voit luoda uuden tiedoston Bashin avulla käyttämällä "```Bash touch uusi_tiedosto.txt ```". Tämän jälkeen voit listata uuden tiedoston sisällön käyttämällä "```Bash ls -l uusi_tiedosto.txt ```". Komento "ls" tarkoittaa listaa ja "l" flagi näyttää lisätiedot tiedostosta.

Output:
```
-rw-r--r--  1 username groupname      0 Dec 31 12:00 uusi_tiedosto.txt
```

Haluatko lisätä sisältöä uuteen tiedostoon? Voit tehdä sen käyttämällä "```Bash echo "Tämä on uuden tiedoston sisältö" >> uusi_tiedosto.txt ```". Tässä komennossa "echo" tulostaa halutun tekstin ja ">>" lisää sen uuden tiedoston loppuun.

Output:
```
-rw-r--r--  1 username groupname     26 Jan  1 12:00 uusi_tiedosto.txt
```

## Syvenny Bash-tekstitiedostojen kirjoittamiseen

Bash-koodin avulla voit tehdä paljon enemmän kuin vain luoda ja muokata tekstitiedostoja. Voit esimerkiksi kirjoittaa skriptejä, jotka ajavat useita komentoja peräkkäin tai jopa ajastaa tietyt toiminnot tietokoneellesi. Voit myös käyttää muuttujia Bashissa helpottaaksesi koodin uudelleenkäyttöä ja tehdä siitä joustavamman.

Bash-kieltä käytettäessä on tärkeää muistaa, että se on herkkä välilyönneille ja erikoismerkeille. Jos esimerkiksi luot uuden tiedoston komennolla "```Bash touch uusi tiedosto.txt ```", komento ei toimi, koska siinä on välilyöntejä tiedoston nimen välissä. Sen sijaan voit käyttää alaviivoja tai sisäänrajauksia, kuten "uusi_tiedosto.txt" tai "uusi\ tiedosto.txt".

## Katso myös

- [Bashin perusteet](https://linuxjourney.com/lesson/bash-basics)
- [Linuxin komentorivin käyttöohje](https://linux.die.net/man/)
- [Bash-opasjaksoja](https://www.shellscript.sh/index.html)