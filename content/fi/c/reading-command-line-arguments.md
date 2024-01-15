---
title:                "Mainostoimiston tehtävä"
html_title:           "C: Mainostoimiston tehtävä"
simple_title:         "Mainostoimiston tehtävä"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lukea komentorivin argumentteja? Yksinkertaisesti sanottuna, se on tärkeä taito, jota tarvitaan monissa C-ohjelmoinnin sovelluksissa. Komennen on mahdollista hyödyntää käyttäjän syötteitä ja parametreja, jotta ohjelma voi toimia eri tavoin kunkin tilanteen mukaan. Tämä tekee ohjelmasta joustavamman ja helpommin hallittavan.

## Kuinka tehdä se?

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    // Tämä koodinpätkä tulostaa komentorivin argumentit
    // yksi kerrallaan, alkaen ensimmäisestä argumentista (argv[1])
    // argv[0] sisältää tiedoston nimen, joten se jätetään huomioimatta

    printf("Sain %d argumenttia \n", argc - 1);

    for (int i = 1; i < argc; i++)
    {
        printf("Argumentti %d: %s \n", i, argv[i]);
    }

    return 0;
}
```

**Syöte:**

```
OhjelmanNimi argumentti1 argumentti2 argumentti3
```

**Tulos:**

```
Sain 3 argumenttia
Argumentti 1: argumentti1
Argumentti 2: argumentti2
Argumentti 3: argumentti3
```

Kuten koodista näkyy, ensimmäisessä argumentissa on tiedoston nimi, joten itse argumentit alkavat indeksistä 1. Nämä argumentit voidaan tallentaa muuttujiin ja käyttää halutulla tavalla ohjelmassa.

## Syvemmälle aiheeseen

Komennorivin argumentteja käsittelevä osa C-ohjelmoinnissa on osa standardikirjastoa stdlib.h. Tämä mahdollistaa argumenttien käsittelyn kaikissa C-ohjelmissa samantyyppisellä koodilla. 

Lisäksi, komentorivin argumenttien lukumäärä ja tiedot voi tarkistaa esimerkiksi käyttämällä funktiota `argc` ja `argv`. `argc` sisältää argumenttien lukumäärän, ja `argv` on osoitin ensimmäiseen argumenttiin.

Lisätietoja löytyy [C-kielen dokumentaatiosta](https://devdocs.io/c/).

## Katso myös

[C-kielen dokumentaatio](https://devdocs.io/c/)

[Komentorivin argumenttien käsittely Bash-skripteillä](https://www.linode.com/docs/tools-reference/tools/how-to-use-arguments-in-a-bash-script/)