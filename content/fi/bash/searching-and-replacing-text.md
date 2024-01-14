---
title:                "Bash: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko tehdä nopeita ja tehokkaita muutoksia tekstiin? Haluatko korjata useita sanoja tai lauseita samanaikaisesti? Sitten tekstinhaku ja korvaaminen voi olla juuri oikea ratkaisu sinulle. Se on helppo tapa muokata suuria määriä tekstiä helposti ja nopeasti.

## Kuinka

Bash-ohjelmoinnissa on useita tapoja etsiä ja korvata tekstiä. Yksi yleisimmistä tavoista on käyttää "sed" -komentoa, joka on lyhenne sanalle "stream editor". Se näyttää tältä:

```Bash
sed 's/etsitty teksti/korvaava teksti/g' tiedostonimi
```

Tässä komennossa "s" viittaa etsittävään tekstiin ja "g" viittaa sanojen tai lauseiden korvaamiseen koko tiedostossa. Voit myös käyttää muita säännöllisiä lausekkeita korvaamiseen, kuten esimerkiksi korvaamaan kaikki numerot tiedostossa:

```Bash
sed 's/[0-9]/#/g' tiedostonimi
```

Toinen tapa etsiä ja korvata tekstiä on käyttää "tr" -ohjelmaa, joka on lyhenne sanalle "translate". Se näyttää tältä:

```Bash
tr 'etsitty teksti' 'korvaava teksti' < tiedostonimi
```

Tämä komento korvaa vain ensimmäisen esiintymän jokaisesta etsitystä sanasta tai lauseesta. Voit myös käyttää säännöllisiä lausekkeita "tr" -komennossa.

## Syvällisempi tarkastelu

Etsimisen ja korvaamisen lisäksi Bash-ohjelmoinnissa on useita muita tapoja muokata tekstiä. Voit muun muassa käyttää "grep"-komennolla etsiä tietynlaista tekstiä ja sitten "cut"-komennolla poistaa halutut osat. Voit myös yhdistellä erilaisia komentoja saadaksesi tarkempia muokkausmahdollisuuksia.

On myös hyödyllistä ymmärtää säännöllisiä lausekkeita ja niiden käyttöä haku- ja korvaustoiminnoissa. Ne antavat sinulle enemmän joustavuutta ja tarkempaa kontrollia muokkauksiin.

## Katso myös

- [Bash Cheat Sheet](https://www.educative.io/blog/bash-shell-command-cheat-sheet)
- [Perusteet Bash-skriptauksesta](https://linuxjourney.com/lesson/bash-scripting-basics)
- [Säännölliset lausekkeet - yksinkertainen opas](https://www3.ntu.edu.sg/home/ehchua/programming/howto/Regexe.html)