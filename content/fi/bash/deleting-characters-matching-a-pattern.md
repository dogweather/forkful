---
title:                "Bash: Mallin mukaisen merkkien poistaminen"
simple_title:         "Mallin mukaisen merkkien poistaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa tiettyä kaavaa vastaavat merkit?

Usein Bash-ohjelmoinnissa törmätään tilanteisiin, joissa halutaan poistaa tiettyjä merkkejä tiedostojen tai syötteiden käsittelyssä. Tämä voi olla tarpeellista esimerkiksi tiettyjen tiedostojen järjestelyssä tai tietojen puhdistamisessa ennen niiden käsittelyä. Tässä artikkelissa tarkastelemme, miten Bashilla voidaan poistaa merkkejä, jotka vastaavat tiettyä kaavaa.

## Miten tehdä

Bashissa on useita tapoja poistaa merkkejä peräkkäisistä merkeistä ja sanoista. Yksi tapa on käyttää `sed`-komennon `s`-vaihtoehtoa. Seuraavassa esimerkissä poistamme tietyn kaavan vastaavat merkit "Hello World" -merkkijonosta ja tulostamme lopputuloksen:

```Bash
echo "Hello World" | sed 's/o//g'
```

Tulostus:

```
Hell Wrld
```

Käytämme `sed`-komennon `s`-vaihtoehtoa, joka tarkoittaa "korvaa". Kaava korvaa kaikki "o"-kirjaimet tyhjällä merkillä (`//`) ja `g` tarkoittaa, että muutos tehdään kaikkiin linjan kohtiin. Voit käyttää myös muita kaavoja, kuten sulkua `()` korvaamaan tiettyjen merkkijonojen osia.

Toinen tapa poistaa merkkejä Bashissa on käyttää `tr`-komennolla `d`-vaihtoehtoa. Esimerkiksi seuraavassa esimerkissä poistamme "o"-kirjaimen "Hello World" -merkkijonosta:

```Bash
echo "Hello World" | tr -d 'o'
```

Tulostus:

```
Hell Wrld
```

Tässä käytämme `tr`-komennon `d`-vaihtoehtoa, joka tarkoittaa "poista". Voit myös käyttää muita vaihtoehtoja, kuten `s` korvaamaan merkkejä tietyn kaavan mukaan.

## Syventyminen

Bashilla merkkien poistaminen on yksinkertaista, mutta se voi silti olla hyödyllinen taito monissa tilanteissa. Voit esimerkiksi käyttää näitä menetelmiä puhdistaaksesi tietoja ennen niiden käsittelyä tai järjestelläksesi tiedostoja kätevästi. Voit myös poistaa merkkejä tietystä kaavasta, kuten numerot tai kirjaimet, käyttämällä säännöllisiä lausekkeita.

On myös hyvä muistaa, että Bashin lisäksi voit käyttää muita työkaluja, kuten Pythonia tai Perl:ia, merkkien poistamiseen. Valitse itsellesi sopivin työkalu ja käytä sitä tehokkaasti tietojen käsittelyyn.

## Katso myös

- [Linux.com: Manipulating Strings in Bash](https://www.linux.com/topic/desktop/manipulating-strings-bash/)
- [The Geek Stuff: Bash String Manipulation Examples – Length, Substring, Find and Replace](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/)
- [Bash-hakemisto Linux.fi-wikissa](https://www.linux.fi/wiki/Bash-hakemisto)