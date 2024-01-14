---
title:                "Fish Shell: Alirivien erottelu"
simple_title:         "Alirivien erottelu"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan halunnut ottaa tietyn osan merkkijonosta ja käyttää sitä erillisenä muuttujana? Tai ehkä haluat vain muokata merkkijonoa tiettynä osana. Tässä blogikirjoituksessa käymme läpi, kuinka voit tehdä näin käyttämällä Fish Shellin ominaisuutta virkkeitä, eli Substring Extractingia.

## Kuinka

Koodinäytteet ja esimerkkilähtö "```Fish Shell ...```" koodilohkoissa.

### Yksinkertainen esimerkki

Oletetaan, että meillä on merkkijono "Tämä on esimerkki". Voimme käyttää seuraavaa komentoa, joka ottaa substrängin "esimerkki" ja sijoittaa sen muuttujaan "näyte".

```
set näyte "Tämä on esimerkki"[9..16]
```

Käyttäen näytettä tulostamme sen konsolille seuraavalla komennolla:

```
echo $näyte
```

Tämä tulostaa "esimerkki".

### Käyttäen muuttujaa

Voimme myös käyttää muuttujaa substrängin määrittämiseen komennon aikana. Esimerkiksi, jos haluamme ottaa merkkijonon viimeisen neljän merkin ja sijoittaa ne uuteen muuttujaan "loppu", voisimme käyttää seuraavaa komentoa:

```
set alkuperäinen "Fish Shell on upea."
set loppu $alkuperäinen[-4..-1]
```

Yllä oleva koodi luo uuden muuttujan "loppu" arvolla "pea.". Voimme sitten tulostaa tämän muuttujan konsolille käyttämällä echo-komentoa.

```
echo $loppu
```

Tämä tulostaa "pea.".

## Syväsukellus

Fish Shellin Substring Extracting on todella kätevä työkalu, jos haluat leikata tai käyttää osia merkkijonosta erillisenä muuttujana. Voit myös yhdistellä erilaisia komentoja tai käyttää muuttujia määrittämään substrängin.

Voit esimerkiksi ottaa merkkijonosta tiettyjä sanoja ja muokata niitä käyttämällä tekstinkäsittelykomentoja, kuten sed ja awk.

## Katso myös

- [Fish Shellin virpukot](https://fishshell.com/docs/current/tutorial.html#tut_home)
- [Substring Extracting -opas](https://fishshell.com/docs/current/index.html#substring-extracting)