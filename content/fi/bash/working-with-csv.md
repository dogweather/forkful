---
title:                "Työskentely csv-tiedostojen kanssa"
html_title:           "Bash: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

CSV-tiedostot ovat yleinen tapa tallentaa taulukkomuotoista dataa, kuten tietokantojen tiedot tai Excelin taulukot. Koodarin näkökulmasta CSV:t ovat käteviä, koska ne ovat helppo luoda ja lukea. Useimmat koodauskielit tukevat CSV-tiedostoja, joten niitä voidaan hyödyntää monenlaisissa sovelluksissa.

## Miten:

Bashin toiminnot CSV-tiedostojen käsittelyyn perustuvat erityisesti komentoriviin ja sen eri työkaluihin. Esimerkiksi `echo`-komennolla voit kirjoittaa CSV-tiedoston sisällön komentoriville ja `cut`-komennolla voit leikata haluamasi sarakkeet tiedostosta. Alla on nähtävillä esimerkki CSV-tiedoston sisällöstä ja sen käsittelystä käytännössä.

```Bash
# CSV-tiedoston sisältö (tiedoston nimi: tiedosto.csv)

Nimi, Ikä, Ammatti
Juha, 25, Koodari
Maija, 30, Insinööri
Matti, 40, Kirjailija
```

```Bash
# Haetaan tietty sarakkeet tiedostosta ja tulostetaan ne komentoriville

cut -d, -f 1,3 tiedosto.csv
```

```
Nimi, Ammatti
Juha, Koodari
Maija, Insinööri
Matti, Kirjailija
```

## Syväsukellus:

CSV-tiedostojen historia juontaa juurensa 1970-luvulle, jolloin ne kehitettiin vastauksena kasvavaan tarpeeseen datan tallentamiseen taulukkomuodossa. Nykyään on olemassa myös muita vaihtoehtoja CSV:lle, kuten XML tai JSON, mutta CSV:t ovat edelleen suosittuja niiden yksinkertaisuuden ja helppokäyttöisyyden vuoksi. Bashissa CSV-tiedostojen käsittelyyn on banyakirjo kirjastoja ja työkaluja, kuten `awk` ja `sed`. Näitä kannattaa tutkia lisää, mikäli CSV:t ovat olennainen osa koodin työstöä.

## Katso myös:

- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [CSV Wikipedia](https://fi.wikipedia.org/wiki/CSV)