---
title:                "Työskentely yaml:n kanssa"
html_title:           "Go: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

Mitä ja Miksi?

Go on ohjelmointikieli, joka on yhä suositumpi valinta monien ohjelmoijien keskuudessa. Yksi syy tähän on sen kyky työskennellä YAML-muotoisten tiedostojen kanssa. YAML on tietojen tallennusmuoto, joka on helppo lukea ja ymmärtää ihmisen silmin, mutta myös helposti käsiteltävissä ohjelmointikielellä.

Miten tehdä:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v2"
	"io/ioutil"
)

type Person struct {
	Name        string
	Age         int
	Interests   []string
}

func main() {
	p := Person{Name: "Matti", Age: 25, Interests: []string{"Ohjelmointi", "Kirjoittaminen", "Ruuanlaitto"}}
	d, err := yaml.Marshal(&p)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(string(d))

	err = ioutil.WriteFile("person.yaml", d, 0644)
	if err != nil {
		fmt.Println(err)
	}
}
```
Tämä koodi esittelee yksinkertaisen esimerkin Go:n käytöstä YAML-tiedostojen kanssa. Ensimmäisessä vaiheessa luodaan Person-rakenne, joka sisältää henkilön nimen, iän ja kiinnostuksen kohteet. Sitten tämä rakenne muunnetaan YAML-muotoon ja tulostetaan konsoliin sekä tallennetaan tiedostoon nimeltä "person.yaml". 
Tuloksena saadaan seuraava YAML-tiedosto:

```
name: Matti
age: 25
interests:
- Ohjelmointi
- Kirjoittaminen
- Ruuanlaitto
```

Syvällisempi sukeltaminen:

YAML julkaistiin vuonna 2001 ja sen tarkoituksena on tarjota helppolukuinen vaihtoehto JSON- ja XML-tiedostoille. Vaikka se onkin aloittanut lähinnä Python-yhteisössä, se on nykyään laajalti käytössä monissa muissa ohjelmointikielissä, mukaan lukien Go. 
Yksi vaihtoehto YAML:lle on TOML (Tom's Obvious, Minimal Language), joka on todella minimalistinen tiedostomuoto, ja sitä käytetään erityisesti konfigurointitiedostojen kanssa. Yhteensopivien kirjastojen avulla Go:n avulla voit helposti työskennellä myös TOML-tiedostojen kanssa. 
Tämä esimerkki näyttää vain perusteet, jotka auttavat sinua pääsemään alkuun Go:n käytössä YAML-tiedostojen kanssa. Kuitenkin tarkempi tutkimus auttaa sinua löytämään lisää hyödyllisiä toimintoja ja niksejä työskennellessäsi YAML-tiedostojen kanssa.

Linkkejä:

- YAML-dokumentaatio: https://yaml.org/
- TOML-dokumentaatio: https://toml.io/
- Gopkg.in/yaml.v2-dokumentaatio: https://pkg.go.dev/gopkg.in/yaml.v2