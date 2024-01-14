---
title:    "Bash: Alaryhmien erottaminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluat oppia kuinka eristetään osamerkkijonoja?

On monia syitä, miksi joku haluaisi hallita osamerkkijonoja Bash-ohjelmoinnissa. Yksi yleisimmistä syistä on käsitellä merkkijonoja, jotka sisältävät enemmän tietoa kuin mitä tarvitset tai tarvitset tietyn osan merkkijonosta tarkemman analyysin suorittamiseksi.

## Kuinka: Näin eristät osamerkkijonoja Bash-ohjelmoinnissa!

Bashilla on useita erilaisia tapoja eristää osamerkkijonoja. Yksi yleisimmistä tavoista on käyttää `grep` -komentoa.

```
Bash on muotoiltu kieli, joka on hyödyllinen
```

Lähtö:

```
muotoiltu kieli
```

Voit myös käyttää `cut` -komentoa eristämään osamerkkijonoja tietyillä välimerkeillä.

```
Bash on hyödyllinen kieli, joka on muotoiltu
```

Lähtö:

```
kieli
```

Lisäksi, Bashilla on myös mahdollista käyttää regex (Regular Expression) -lausekkeita osamerkkijonojen eristämiseen. Esimerkiksi jos haluat eristää kaikki numerot merkkijonosta, voit käyttää seuraavaa komentoa:

```
echo "123aBCd" | grep -o '[0-9]*'
```

Lähtö:

```
123
```

Näitä ovat vain muutamia esimerkkejä siitä, kuinka osamerkkijonoja voidaan eristää Bashilla. Kokeile ja löydä menetelmä, joka toimii parhaiten sinulle ja tarpeisiisi.

## Syvemmälle: Tietoa osamerkkijonojen eristämisestä Bashilla

Kuten aiemmin mainittiin, Bashilla on mahdollista käyttää regex-lausekkeita osamerkkijonojen eristämiseen. Tämä avaa monia mahdollisuuksia tarkempaan ja monipuolisempaan merkkijonojen käsittelyyn.

Regex-lausekkeet ovat sääntöjä, jotka kuvaavat, millaista tekstiä etsitään. Ne voivat sisältää erilaisia hakuehtoja ja erityisiä merkkijonoja, joilla määritellään haettavaa tekstiä.

Bash tarjoaa myös muita hyödyllisiä komentoja, kuten `sed` ja `awk`, joita voidaan käyttää osamerkkijonojen käsittelyyn. Näitä komentoja voidaan yhdistää regex-lausekkeisiin, jotta erilaisia osia merkkijonosta voidaan eristää ja manipuloida.

## Katso myös:
- https://linuxhint.com/extract_substring_bash/
- https://linuxize.com/post/bash-extract-substring-from-string/
- https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- https://www.computerhope.com/unix/bash/replacing.html