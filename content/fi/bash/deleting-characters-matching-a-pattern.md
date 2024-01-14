---
title:    "Bash: Kuviota vastaavien merkkien poistaminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Usein Bash-ohjelmoijat joutuvat tekemään erilaisia muutoksia merkkijonoihin. Toisinaan näihin muutoksiin kuuluu myös tietynlaisten merkkien poistaminen. Tämä voi olla tarpeellista esimerkiksi tietokantaan tallennetuissa tiedoissa tai lokaalissa tiedostossa. Seuraavassa osiossa kerromme miten tätä voi tehdä Bashin avulla.

## Miten

Merkkien poistaminen halutunmallisen patternin mukaisesti onnistuu Bashilla yksinkertaisesti käyttämällä komentoa "tr". Komento "tr" vaihtaa, poistaa tai lisää merkkejä merkkijonosta määritetyn patternin mukaan. Seuraava esimerkki osoittaa miten voit poistaa kaikki numerot merkkijonosta:

```Bash
sana="Tämä on 123 testi"
uusi_sana=$(echo $sana | tr -d '[0-9]')
echo $uusi_sana # Tulostaa: Tämä on testi
```

Kuten huomaat, käytämme ensin "echo" komentoa tulostaaksemme merkkijonon ja sitten "tr" komentoa poistaaksemme kaikki numerot merkkijonosta. Voit myös lisätä muita merkkejä patterniin, jos haluat poistaa tai korvata niitä.

## Syvempi sukellus

Komennon "tr" määritelty pattern on käytännössä sallitut merkit, jotka korvataan niin haluttaessa. Voit myös määritellä useampia patternia ja näin korvata useampia merkkejä tai merkkijonoja. Voit myös käyttää "tr" komentoa skripteissäsi, jolloin voit automatisoida merkkien poistamisen tai korvaamisen.

## Katso myös

- [Bashin "tr" komento dokumentaatio](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Merkkijonojen muokkaaminen Bashilla](https://bash.cyberciti.biz/guide/Perform_an_action_on_all_files_in_a_directory)
- [Muita Bashin hyödyllisiä komentoja](https://devhints.io/bash)