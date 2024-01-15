---
title:                "Å jobbe med yaml"
html_title:           "Go: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med YAML når man programmerer i Go? Vel, YAML er et strukturert og lettleselig dataformat som brukes for å konfigurere og definere data. Dette gjør det ideelt for å håndtere konfigurasjonsfiler i Go-applikasjoner.

## Hvordan

For å begynne å jobbe med YAML i Go, må du først importere "gopkg.in/yaml.v2" pakken. Deretter kan du bruke "yaml.Marshal()" funksjonen for å konvertere et Go-objekt til YAML-format. Her er et eksempel på hvordan du kan konvertere et enkelt tall til YAML og skrive det ut:

```Go
package main

import (
	"fmt"

	"gopkg.in/yaml.v2"
)

func main() {
	num := 42
	yml, err := yaml.Marshal(num)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(string(yml))
}
```

Output:

```
42
```

Du kan også konvertere et komplekst Go-objekt som inneholder en struct til YAML ved å bruke "yaml.Marshal()" funksjonen. Her er et eksempel på en struct som inneholder informasjon om en person og hvordan du kan konvertere den til YAML:

```Go
type Person struct {
	Name    string
	Age     int
	Country string
}

func main() {
	p := Person{
		Name:    "Maria",
		Age:     27,
		Country: "Norway",
	}
	yml, err := yaml.Marshal(p)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(string(yml))
}
```

Output:

```
name: Maria
age: 27
country: Norway
```

## Deep Dive

Når du jobber med YAML i Go, kan det være nyttig å vite noen av de viktigste strukturene og funksjonene som brukes. En YAML-struktur består av nøkler og verdier, og kan være en liste eller et map. Disse strukturene kan også være innebygd i hverandre for å lage mer kompleks datastruktur.

I Go, kan du bruke "map[string]interface{}" for å representere et map, og "[]interface{}" for å representere en liste. "interface{}" betyr at datatypen kan være hva som helst, noe som gjør det fleksibelt for å håndtere forskjellige typer data.

En annen nyttig funksjon når du jobber med YAML er "yaml.Unmarshal()". Denne funksjonen lar deg konvertere YAML til et Go-objekt. Her er et eksempel på hvordan du kan bruke "yaml.Unmarshal()" for å konvertere YAML til en struktur og deretter få tilgang til dataene i strukturen:

```Go
type Car struct {
	Brand  string
	Model  string
	Year   int
	Engine struct {
		Type       string
		Horsepower int
	}
}

func main() {
	yml := `
brand: BMW
model: M5
year: 2021
engine:
  type: V8
  horsepower: 600
`
	c := Car{}
	err := yaml.Unmarshal([]byte(yml), &c)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(c.Brand)
	fmt.Println(c.Engine.Type)
}
```

Output:

```
BMW
V8
```

## Se også

- [YAML Offisiell Hjemmeside] (https://yaml.org/)
- [Go Dokumentasjon for "gopkg.in/yaml.v2" pakken] (https://pkg.go.dev/gopkg.in/yaml.v2)
- [Go YAML-parser sammenligning] (https://medium.com/@shijuvar/go-yaml-parsers-comparison-39c0eeb6ac12)