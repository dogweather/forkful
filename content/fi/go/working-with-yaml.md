---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "YAML-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML on datan sarjoittamiskieli automatisointiin, konfigurointiin ja muuhun. Ohjelmoijat käyttävät YAML:ia, koska se on ihmisen luettavissa ja sen rakenne on koneellisesti käsiteltävä.

## How to:
### YAML:n Käsittely Go:lla
Käytetään `gopkg.in/yaml.v3` kirjastoa YAML:n käsittelyyn Go:ssa. Ensin asennetaan kirjasto:

```Go
go get gopkg.in/yaml.v3
```

Seuraava esimerkki näyttää, miten luetaan ja kirjoitetaan YAML-tiedostoja.

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
	"io/ioutil"
	"log"
)

// Määritellään konfiguraatiotyyppi
type Config struct {
	Palvelin string `yaml:"palvelin"`
	Portti   int    `yaml:"portti"`
}

func main() {
	// Luetaan YAML-tiedosto
	data, err := ioutil.ReadFile("config.yaml")
	if err != nil {
		log.Fatal(err)
	}

	// Unmarshal YAML-data Go:n rakenteeseen
	var conf Config
	err = yaml.Unmarshal(data, &conf)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Palvelin: %s\nPortti: %d\n", conf.Palvelin, conf.Portti)

	// Muutetaan Go-rakenteen arvoja
	conf.Portti = 8080

	// Marshal uudet arvot takaisin YAML-muotoon
	uusiData, err := yaml.Marshal(&conf)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(uusiData))

	// Kirjoita muutokset takaisin tiedostoon
	err = ioutil.WriteFile("config.yaml", uusiData, 0644)
	if err != nil {
		log.Fatal(err)
	}
}

```
Esimerkissä luetaan konfiguraatiot `config.yaml`-tiedostosta, muutetaan portaali ja kirjoitetaan päivitetyt tiedot takaisin.

## Deep Dive
YAML, lyhenne sanoista YAML Ain't Markup Language, julkaistiin ensin 2001. Sen rakenne yksinkertaistaa JSON:ista. YAML:n kilpailijoita ovat JSON ja XML. Vaihtoehtoisesti voidaan harkita TOML:ia tai INI-tiedostoja yksinkertaisiin asetuksiin. Go:n `yaml`-kirjastot eivät tyypillisesti tarkkaile YAML-luonnoksen viimeisimpiä versioita, mutta ne ovat silti hyvin yhteensopivia useimpien YAML-tiedostojen kanssa.

## See Also
- YAML-spesifikaatio: [YAML 1.2](https://yaml.org/spec/1.2/spec.html)
- `yaml.v3` Go-paketti: [gopkg.in/yaml.v3](https://gopkg.in/yaml.v3)
- Go-dokumentaatio: [Go](https://golang.org/doc/)
- Tutki lisää JSON:ista ja XML:stä: [JSON](https://www.json.org/json-en.html), [XML](https://www.w3.org/XML/)
