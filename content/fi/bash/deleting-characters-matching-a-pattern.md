---
title:                "Kuvion vastaavien merkkien poistaminen"
html_title:           "Bash: Kuvion vastaavien merkkien poistaminen"
simple_title:         "Kuvion vastaavien merkkien poistaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa tulee tilanteita, joissa haluamme poistaa tietyt merkit tai merkkijonot koodista. Tämä voi olla esimerkiksi turhien välimerkkien poistaminen tai tiettyjen haitallisten merkkien suodattaminen. Bash-skriptien avulla tämä onnistuu helposti ja tehokkaasti.

## Miten

Bash tarjoaa joukon käteviä komentoja poistamaan merkkejä tai merkkijonoja halutun kaavan perusteella. Käytämme seuraavia komentoja poistaaksemme merkkejä:

```Bash 
# Poistaa kaikki numerot merkkijonosta "hello123world"
echo "hello123world" | tr -d [:digit:] 
# Output: helloworld
```

Käytännössä komento ```tr``` (lyhenne sanasta "translate") korvaa kaikki annetun kaavan mukaiset merkit tyhjällä merkillä, jolloin ne poistuvat kokonaan merkkijonosta. Näin ollen se sopii hyvin poistamaan tiettyjä merkkejä tai merkkijonoja.

```Bash
# Poistaa kaikki isoilla kirjaimilla kirjoitetut sanat merkkijonosta "Bash is AMAZING"
echo "Bash is AMAZING" | tr -d [:upper:] 
# Output: is 
```

Käyttämällä komentoa ```sed``` (lyhenne sanasta "streamline editor"), voimme myös poistaa tiettyjä merkkijonoja tai sanoja laajemmin. Käytämme tällöin säännöllisiä lausekkeita (regular expressions) määrittelemään poistettavat merkkijonot.

```Bash
# Poistaa kaikki viisi kirjainta pidemmät sanat merkkijonosta "This article is awesome"
echo "This article is awesome" | sed 's/\b[A-Za-z]\{5,\}\b//g' 
# Output: This is 
```

Tässä tapauksessa komento ```sed``` etsii sanoja, jotka ovat vähintään viisi kirjainta pitkiä ja korvaa ne tyhjällä merkillä. Tämän ansiosta esimerkiksi sanat "article" ja "awesome" poistuvat kokonaan merkkijonosta.

## Syvemmälle

Kuten edellisistä esimerkeistä näkyy, Bash tarjoaa monipuolisia ja tehokkaita keinoja poistaa tarpeettomia merkkejä tai merkkijonoja koodista. On myös hyvä huomata, että komentojen yhdistelemällä ja erilaisia säännöllisiä lausekkeita käyttämällä pystytään ratkaisemaan monimutkaisempiakin ongelmia.

Lisäksi Bash-skripteihin on mahdollista luoda myös omia funktioita tai käyttää valmiita komentoriviohjelmia, kuten "awk" tai "grep", poistamiseen. Tästä syystä kannattaa tutustua Bashin eri ominaisuuksiin ja mahdollisuuksiin syvemmin ja löytää juuri itselle sopiva tapa poistaa merkkejä koodista.

## Katso myös

- [Bash-skriptikielen virallisilta sivuilta](https://www.gnu.org/software/bash/)
- [Bash-skriptin kirjoittamisen perusteet](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
- [Käyttäjän määrittelemien funktioiden luominen Bashissa](https://www.shellscript.sh/functions.html)