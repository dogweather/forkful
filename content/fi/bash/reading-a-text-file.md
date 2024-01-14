---
title:    "Bash: Tekstitiedoston lukeminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat lukea teksti-tiedostoja Bash-ohjelmoinnissa. Ehkä haluat käyttää niitä luomaan dynaamisia skriptejä, joissa vaihtelevaa sisältöä käsitellään eri tavoin. Tai ehkä haluat tarkastella tai muuttaa tiedoston tietoja käytön jälkeen.

## Miten

Voit lukea teksti-tiedostoja Bash-skripteissäsi käyttämällä `cat`-komennon yhdistelmää `while`-silmukalla. Katso seuraava esimerkki:

```Bash
#!/bin/bash
cat tiedosto.txt | while read line
do
	echo "Rivi sisältää: $line"
done
```

Tässä esimerkissä käytämme `cat`-komennon avulla lukeaksemme tiedoston ja sitten `while`-silmukassa luomme toiminnon jokaiselle riville. Voit käyttää `read`-komentoa `while`-silmukassa lukeaksesi yhden rivin kerrallaan. Tämä mahdollistaa rivien käsittelyn haluamallasi tavalla, kuten tulostuksen tai tallennuksen muuttujaan.

## Syventävä sukellus

Käytämme `cat`-komentoa lukemaan tiedoston sisällön ja `while`-silmukka `read`-komennolla lukemaan jokaisen rivin. On kuitenkin myös muita vaihtoehtoja. Voit käyttää `grep`-komennon avulla tietyt merkkijonot tai rivit tiedostosta, tai `sed`-komennon avulla muokata tiedoston sisältöä.

Lisäksi voit käyttää muita ohjelmia, kuten `awk` tai `cut`, jotta voit käsitellä tiettyjä sarake- tai kenttäarvoja tiedostosta. Voit myös käyttää `while`-silmukan sijaan `for`-silmukkaa tai `for`-silmukkaa, jos tiedät tarkalleen haluamasi rivien määrän, jota käsittelet tiedostossa.

## Katso myös

- [Bash Scripting Basics](https://www.tutorialspoint.com/unix/advanced_bash_scripting.htm)
- [The Power of Bash's Built-in Commands](https://linuxacademy.com/blog/linux/the-power-of-bashs-built-in-commands/)