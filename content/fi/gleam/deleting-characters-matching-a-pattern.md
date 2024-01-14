---
title:                "Gleam: Pohdinta kielioppia: Merkinpoistaminen vastaavaa mallia käyttäen"
simple_title:         "Pohdinta kielioppia: Merkinpoistaminen vastaavaa mallia käyttäen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
Tulee usein tilanteita, jossa ohjelmointikielessä käytetyn merkkijonon joukosta halutaan poistaa tietyllä tavalla valikoituja merkkejä. Tämä voi olla tarpeellista esimerkiksi datan käsittelyssä tai tietyn toiminnon suorittamisessa. Gleam-ohjelmointikielen avulla tämä on helppoa ja tehokasta.

## Kuinka tehdä
Käytännön esimerkki poistettaessa merkkejä, jotka vastaavat tiettyä kaavaa (pattern):

```Gleam
import gleam/regex

let string = "Tervetuloa Gleamin ihmeelliseen maailmaan!"
let pattern = regex.compile("\\d+")
let result = regex.replace_all(pattern, string, "")
```

Tässä ensiksi tuodaan mukaan Gleam-kielen regex-kirjasto ja luodaan merkkijono, josta haluamme poistaa merkkejä. Sitten määritellään haluttu kaava, joka tässä tapauksessa etsii numerollisia merkkejä. Lopuksi käytetään regex-kirjaston toimintoa replace_all, joka poistaa kaikki löytyneet merkit ja palauttaa lopullisen tuloksen.

Tässä on sample output tuloksena yllä olevasta koodista:

```
Tervetuloa Gleamin ihmeelliseen maailmaan!
```

Voit vaihtaa kaavaa ja merkkijonoa testataksesi eri tuloksia.

## Syvällisempää tietoa
Gleam-kielen regex-kirjasto tarjoaa erilaisia toimintoja merkkijonojen käsittelyyn. Esimerkiksi löytyy myös funktiot kuten replace_first, joka poistaa vain ensimmäisen löydetyn merkin tai split, joka jakaa merkkijonon useaksi pienemmäksi osaksi halutun merkin kohdalta.

Jokaisella regex-toiminnolla on myös monipuoliset vaihtoehdot ja mahdollisuus käyttää säännöllisiä lausekkeita. Tämä antaa suuren joustavuuden ja tarkkuuden merkkijonojen käsittelyyn.

## Katso myös
- [Gleam-kielen virallinen dokumentaatio](https://gleam.run/docs/)
- [Regex-kirjaston dokumentaatio](https://github.com/gleam-lang/regex)
- [Gleam-kielen oppaat ja tutoriaalit](https://github.com/gleam-lang/awesome)