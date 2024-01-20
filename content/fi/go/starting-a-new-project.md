---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen merkitsee uuden ohjelmointitehtävän tai -sovelluksen käynnistämistä alusta alkaen. Ohjelmoijat aloittavat uusia projekteja oppimisen, uusien ratkaisujen löytämisen tai jopa oman tuotteen kehittämisen takia.

## Näin se tehdään:

Go-projektin aloittaminen on suoraviivaista. Seuraavat koodiesimerkit näyttävät, kuinka Go-perusteet tukevat uutta projektia:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hei maailma!")
}
```

Kun suoritat tämän koodin, tulostuu:

```
Hei maailma!
```

## Syvempi katsaus

Historiallisessa merkityksessä Go (tunnetaan myös nimellä Golang) luotiin Googlella vuonna 2007 Robert Griesemerin, Rob Piken ja Ken Thompsonin toimesta. Go:n suunnittelun tärkein tavoite oli luoda kieli, joka mahdollistaa suurten järjestelmien kehittämisen tehokkaasti.

Vaihtoehtoina uudelle Go-projektille voisivat olla kieliä, kuten Python tai JavaScript. Suurin ero on siinä, että Go on staattisesti kirjoitettu ja suorituskykyinen kieli, joka soveltuu erityisesti rinnakkaisten ja jakautuneiden järjestelmien kehittämiseen.

Go:lla uuden projektin aloitus merkitsee usein moduulin luomista. Moduuli on paketti tai useita paketteja, jotka jakavat saman `go.mod` -tiedoston. Se määrittelee moduulin nimen (usein repositorion polku), sen vaatimat go-versions and its dependencies.

## Katso myös:

1. [Go:n virallinen dokumentaatio](https://golang.org/doc/)
2. [Go:n projektin moduulien luominen](https://blog.golang.org/using-go-modules)
3. [Go Tutorial: Aloita Go](https://www.tutorialspoint.com/go/index.htm)

Onnea uuden Go-projektisi kanssa!