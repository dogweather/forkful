---
title:                "Merkkijonon interpolointi"
html_title:           "Haskell: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Interpoloiminen on prosessi, jossa merkkijonoa muokataan lisäämällä siihen dynaamisesti tietoa. Tämä on hyödyllistä, kun halutaan luoda merkkijonoja, jotka sisältävät muuttuvia arvoja. Ohjelmoijat tekevät tätä usein esimerkiksi tulostettaessa tietokannasta haettuja tietoja tai luotaessa HTML-tiedostoja.

# Kuinka tehdä:

Käytä Haskellin ```printf``` funktiota interpolointiin. Se ottaa ensimmäisenä parametrina merkkijonon, jossa haluat sisällyttää arvot ja sen jälkeen tarvittavat arvot pilkulla eroteltuna. Esimerkiksi:

```Haskell
printf "Hei, minun nimeni on %s ja olen %d vuotta vanha!" "Sara" 25
```

Tämä tulostaa:

```
"Hei, minun nimeni on Sara ja olen 25 vuotta vanha!"
```

Voit myös käyttää Haskellin ```strcat``` funktiota yhdistelemään merkkijonoja. Esimerkiksi:

```Haskell
strcat "Tämä viesti sisältää " "useita " "erillisiä " "sanoja!"
```

Tämä tulostaa:

```
"Tämä viesti sisältää useita erillisiä sanoja!"
```

# Syvenny:

Interpoloimisen juuret ovat C-korkean tason ohjelmointikielessä, jossa oli käytössä ```printf```-funktio. Muita vaihtoehtoja interpoloimiseen ovat esimerkiksi Pythonin ```format``` funktio ja JavaScriptin ```template literals```.

Optimaalisen suorituskyvyn saamiseksi Haskellin ```strcat``` käyttää sisäisesti ```StringBuffer```-taulukkoa, jossa muutokset tapahtuvat tehokkaasti tilan varastamisen sijaan.

# Katso myös:

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [C-kielen perusmuotoilu ja tulostus](https://www.gnu.org/software/make/manual/html_node/Formatcmd.html)