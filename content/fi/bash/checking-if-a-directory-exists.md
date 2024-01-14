---
title:                "Bash: Tarkista onko hakemisto olemassa"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

 Bash-ohjelmoinnissa on usein tarve tarkistaa, onko tietty hakemisto olemassa. Tässä blogikirjoituksessa kerromme, miksi tämä tarkistus on tärkeä osa ohjelmointia ja kuinka se tehdään helposti Bash-kielellä.

## Kuinka tarkistaa hakemiston olemassaolo Bashilla

Hakemiston olemassaolon tarkistaminen Bashilla on helppoa käyttämällä `test`-komentoa yhdessä `-d`-valitsimen kanssa. Tämä tarkistaa, onko parametrina annettu polku olemassa ja onko kyseessä hakemisto.

```Bash
if [ -d <hakemiston_polku> ]; then
  echo "Hakemisto on olemassa."
else
  echo "Hakemistoa ei löytynyt."
fi
```

Komento palauttaa 0, jos hakemisto on olemassa, ja muun kuin 0, jos hakemistoa ei löydy. Voit myös tarkistaa hakemiston olemassaolon suoraan if-lausekkeen avulla:

```Bash
if [ -d <hakemiston_polku> ]; then
  echo "Hakemisto on olemassa."
fi
```

## Syvempi sukellus

Bashissa on myös muita tapoja tarkistaa hakemiston olemassaolo, kuten `ls`-komento yhdessä `-d`-valitsimen kanssa, joka listaa tiedoton tiedoston sijasta hakemistojen nimet. Voit myös käyttää `[[`-operaattoria, joka tukee säännöllisiä lausekkeita. Tarkemmat tiedot näistä vaihtoehdoista löydät Bashin dokumentaatiosta.

On myös tärkeää huomata, että hakemiston olemassaolon tarkistaminen ei takaa, että hakemisto on käytettävissä tai luku- ja kirjoituskelpoinen. Tämän vuoksi on suositeltavaa edelleen käyttää tarkistuksia ennen hakemiston käyttöä ohjelmassa.

## Katso myös
- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)