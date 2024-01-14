---
title:                "Fish Shell: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstirivin lukeminen on yksi perustavanlaatuisimmista tehtävistä tiedonkäsittelyssä. Usein meidän täytyy lukea läpi suuria määriä tietoa ja erottaa sieltä haluamamme tiedot. Fish Shell tarjoaa meille helpon tavan tehdä tämä.

## Miten

Fish Shell tarjoaa meille merkkijonotoiminnon `read`. Tällä funktiolla voimme lukea haluamamme tiedoston ja tallentaa sen sisällön muuttujaan. Tämä muuttuja voidaan sitten käyttää haluamallamme tavalla, kuten tulostaa se näytölle.

```Fish Shell
set sisalto (read -i tiedosto.txt)
echo $sisalto
```

Tässä esimerkissä luetaan tiedosto nimeltä "tiedosto.txt" ja tallennetaan sen sisältö muuttujaan nimeltä "sisalto". Sitten tämä sisältö tulostetaan näytölle komennolla `echo`.

## Syvempi sukellus

Lisäksi Fish Shellillä on muitakin vaihtoehtoja tekstirivin lukemiseksi. Voimme myös lukea tiedostosta vain tietyn määrän merkkejä tai rivejä.

```Fish Shell
set merkit (read -n 10 tiedosto.txt)
echo $merkit
```

Tässä esimerkissä luetaan tiedostosta vain 10 ensimmäistä merkkiä ja tallennetaan ne muuttujaan nimeltä "merkit". Sitten tulostetaan muuttujan sisältö näytölle.

## Katso myös
- [Fish Shellin dokumentaatio lukemisesta](https://fishshell.com/docs/current/cmds/read.html)
- [Merkkijonojen käsittely Fish Shellillä](https://fishshell.com/docs/current/tutorial.html#tut_strings)