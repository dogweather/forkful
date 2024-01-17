---
title:                "Alalauseiden poimiminen."
html_title:           "Go: Alalauseiden poimiminen."
simple_title:         "Alalauseiden poimiminen."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Substringien erottaminen (englanniksi "extracting substrings") on yleinen ohjelmoinnin käsite, joka tarkoittaa osajonon poimimista merkkijonosta tai taulukosta. Tätä tehdään yleensä silloin, kun halutaan käsitellä tiettyjä osia merkkijonosta erikseen tai verrata niitä muihin merkkijonoihin.

## Näin:

```Go
// Merkkijonon erottaminen tiettyjen merkkien perusteella
str := "Go on mielenkiintoinen ohjelmointikieli"
substr := str[3:5] // tulos: "on"

// Taulukon erottaminen tietyn pituisilla osilla
arr := []int{1, 2, 3, 4, 5, 6, 7, 8}
substr := arr[2:5] //tulos: [3, 4, 5]
```

## Syvällinen sukellus:

Substringien erottaminen on perusoperaatio, jota käytetään useissa eri ohjelmointitilanteissa. Kehittäjät voivat käyttää myös muita tapoja erottaa merkkijonoja ja taulukoita, kuten regex-merkkijonokäsittelyä tai kirjastofunktioita, mutta substringien erottamisella on usein erilaisia käyttötapoja ja se on usein nopeampi ratkaisu.

## Katso myös:

- [Go-kielen viralliset dokumentaatiot](https://golang.org/doc/)
- [Aloittelijan opas - Opettele Go-kieltä 5 minuutissa](https://learnxinyminutes.com/docs/fi-fi/go-fi)
- [String-tyyppi – Syvempi sukellus merkkijonokäsittelyyn Go-kielessä](https://www.golangprograms.com/golang/string.html)