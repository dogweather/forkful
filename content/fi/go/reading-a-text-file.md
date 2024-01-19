---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tietojen hankkimista tekstipohjaisesta tiedostosta. Ohjelmoijat tekevät tämän, koska se on kätevä tapa säilyttää ja hakea tietoa.

## Miten:

Tässä on yksinkertainen koodiesimerkki, joka näyttää miten tekstitiedosto luetaan Go-ohjelmointikielellä:

```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("test.txt")

	if err != nil {
		log.Fatalf("virhe avattaessa tiedostoa: %v", err)
	}

	scanner := bufio.NewScanner(file)
	
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	
	file.Close()
}
```

Tulostus voisi näyttää tältä, jos test.txt sisältää rivit "Hei" ja "maailma":

```Go
Hei
maailma
```

## Sukellus:

Go:n lukufunktiot ovat osa laajempaa UNIX-filosofiaa: tee yksi asia ja tee se hyvin. Historiallisesti UNIX-järjestelmissä tiedostoja on käytetty tiedonvälitysmekanismina. Go on ottanut tämän mallin ja tuonut sen moderniin ohjelmointimaailmaan.

Kun luet tiedostoa, on olemassa muitakin vaihtoehtoja, kuten `ioutil.ReadFile` tai `ioutil.ReadAll`. Kuitenkin, `bufio.Scanner` on yleensä parempi vaihtoehto, koska se käsittelee muistin tehokkaammin etenkin suurien tiedostojen kanssa.

Itse tekstitiedoston lukemisessa ei ole mitään erikoista. Toki, sinun täytyy olla tietoinen esimerkiksi käytetystä merkistökoodauksesta (esim. UTF-8), mutta Go hoitaa useimmat näistä yksityiskohdista sinulle taustalla. Jos tarvitset enemmän kontrollia, niin Go tarjoaa `Reader` ja `Writer` -rajapinnat, joiden avulla voit tehdä tarkempia toimintoja tiedostojen kanssa.

## Katso myös:

Hyviä resursseja Go-ohjelmointikielen tekstitiedostojen käsittelystä:

1. Go:n virallinen dokumentaatio: [https://golang.org/pkg/bufio/](https://golang.org/pkg/bufio/)
2. Go-by-example -sivuston opetusohjelma: [https://gobyexample.com/reading-files](https://gobyexample.com/reading-files)
3. Hyvä katsaus Go:n tiedostonkäsittelyfunktioista: [https://www.devdungeon.com/content/working-files-go](https://www.devdungeon.com/content/working-files-go)