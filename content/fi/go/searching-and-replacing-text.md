---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Go: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Etsiminen ja tekstin korvaaminen ovat yleisiä ohjelmoijien tekemiä tehtäviä. Tämä tarkoittaa yksinkertaisesti antamasi tekstin sanojen tai lauseiden löytämistä ja korvaamista toisilla sanoilla tai lauseilla. Ohjelmoijat tekevät tätä usein, kun he haluavat muuttaa tai päivittää tiettyä osaa koodistaan.

## Miten?
Go-kielellä etsimisen ja korvaamisen voi tehdä helposti käyttämällä sisäänrakennettua `strings.Replace` -funktiota. Alla on esimerkki, jossa etsimme ja korvaamme sanan "vanha" sanalla "uusi":

```Go
teksti := "Tämä on vanha teksti"
uusiTeksti := strings.Replace(teksti, "vanha", "uusi", 1)
fmt.Println(uusiTeksti) // Tulostaa: "Tämä on uusi teksti"
```

Voimme myös antaa lisäparametrina korvausten määrän, jotta voimme korvata useamman kuin yhden esiintymän. Esimerkiksi:

```Go
teksti := "Korvataan vanha sanonta tässä vanhassa maailmassa"
uusiTeksti := strings.Replace(teksti, "vanha", "uusi", 2)
fmt.Println(uusiTeksti) // Tulostaa: "Korvataan uusi sanonta tässä uudessa maailmassa"
```

## Syvällinen sukellus
Etsimistä ja korvaamista on tehty ohjelmoinnissa jo pitkään, ja eri kielillä on erilaisia tapoja hoitaa tämä tehtävä. Esimerkiksi C-kielellä voidaan käyttää `strstr()` -funktiota, ja Java-kielellä voidaan käyttää `replace()` -metodia. Go-kielessä `strings.Replace` on rakennettu standardikirjaston osaksi, joten se on kätevä ratkaisu tekstien käsittelyyn.

## Katso myös
- [Go:n virallinen dokumentaatio](https://golang.org/pkg/strings/#Replace)
- [Esimerkkejä Go-koodilla](https://gobyexample.com/string-functions)
- [Stringien käsittelyn vertailu eri kielillä](https://benchmarksgame-team.pages.debian.net/benchmarksgame/fastest/go-python3.html)