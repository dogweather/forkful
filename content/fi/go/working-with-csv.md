---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "CSV-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma Separated Values) on tiedostoformaatti, jota käytetään taulukollisen datan tallennukseen. Ohjelmoijat käyttävät CSV:tä sen yksinkertaisuuden ja yhteensopivuuden takia eri ohjelmistojen välillä.

## How to:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	// Luodaan CSV-tiedosto ja kirjoitetaan sinne rivejä
	data := [][]string{
		{"nimi", "ikä", "kaupunki"},
		{"Mikko", "30", "Helsinki"},
		{"Liisa", "25", "Espoo"},
	}

	file, err := os.Create("esimerkki.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, record := range data {
		if err := writer.Write(record); err != nil {
			panic(err)
		}
	}

	// Ladataan CSV-tiedosto ja tulostetaan sen sisältö
	content, err := os.ReadFile("esimerkki.csv")
	if err != nil {
		panic(err)
	}

	reader := csv.NewReader(strings.NewReader(string(content)))
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	for _, record := range records {
		fmt.Println(record)
	}
}
```

## Deep Dive
CSV on ollut käytössä jo vuosikymmeniä, ja se toimii yksinkertaisena mekanismina tietojen siirtämiseen eri järjestelmien välillä. JSON ja XML ovat nykyaikaisia vaihtoehtoja tiedon tallennukseen, mutta ne ovat monimutkaisempia. Go:n `encoding/csv`-kirjastossa hyödynnetään Reader- ja Writer-tyyppejä tiedon lukemiseen ja kirjoittamiseen CSV-muodossa, ja se käsittää myös lainausmerkkien ja välimerkkien käsittelyn.

## See Also
- Go:n virallinen dokumentaatio `encoding/csv`-paketille: https://pkg.go.dev/encoding/csv
- Go by Example - CSV tiedostojen käsittelystä: https://gobyexample.com/csv
- TutorialEdge - CSV-tiedostojen lukeminen ja kirjoittaminen Golla: https://tutorialedge.net/golang/reading-writing-csv-files-in-go/
