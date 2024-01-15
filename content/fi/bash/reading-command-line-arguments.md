---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Bash: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisit komentoriviparametrejä koskevan artikkelin? Komentoriviparametrit tarjoavat mahdollisuuden automatisoida ja nopeuttaa tietokoneen käyttöä, mikä tekee niistä erittäin käteviä ja hyödyllisiä taitoja oppia.

## Miten

Komentoriviparametrit luetaan Bash-skriptien aikana komennon `"$@"` avulla. Alla olevassa esimerkissä tulostetaan kaikki annetut parametrit yksi kerrallaan:

```Bash
#!/bin/bash

for parametri in "$@"
do
    echo "$parametri"
done
```

Jos ajat skriptiä komennolla `./parametrit.sh yksi kaksi kolme`, tulostuu seuraava tulos:

```
yksi
kaksi
kolme
```

Voit myös käyttää komentoriviparametrejä muuttujina muissa komennoissa, kuten alla olevassa esimerkissä, jossa nimen perässä tulostetaan parametriksi annettu sukunimi:

```Bash
#!/bin/bash

for nimi in "$@"
do
    echo "Hae henkilöä nimellä ${nimi} Doe"
    grep "${nimi} Doe" henkilot.txt
done
```

## Syväsukellus

Komentoriviparametrit voidaan myös lukea käyttämällä komentoa `getopts`. Tämä mahdollistaa parametrin vaihtoehtojen, kuten "-l" tai "--lista", lukemisen ja niihin liittyvien toimenpiteiden suorittamisen. Alla olevassa esimerkissä tarkistetaan, onko käyttäjä antanut "-l" parametrin ja jos on, niin tulostetaan lista tiedostoista nykyisessä työskentelyhakemistossa:

```Bash
#!/bin/bash

lista="false"

while getopts "l" vaihtoehto
do
    case "${vaihtoehto}" in
        l)
            lista="true"
            ;;
        \?)
            echo "Virheellinen parametri: ${OPTARG}" >&2
            ;;
    esac
done

if [ $lista == "true" ]
then
    ls
fi
```

Voit ajaa skriptiä komennolla `./lista.sh -l` ja jos olet työskentelyhakemistossasi on esimerkiksi tiedostot "tiedosto1.txt" ja "tiedosto2.txt", näet seuraavan tuloksen:

```
tiedosto1.txt
tiedosto2.txt
```

## Katso myös

- Komentoriviparametrit: https://www.shellscript.sh/tips/getopts/
- Bash-skriptien parametrien lukeminen: https://ryanstutorials.net/bash-scripting-tutorial/bash-parameters.php