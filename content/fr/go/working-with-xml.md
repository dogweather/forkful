---
title:                "Travailler avec XML"
date:                  2024-01-26T04:31:29.452479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec l’XML implique d’analyser, de créer et de manipuler des documents XML en utilisant du code. Les programmeurs le font pour l’échange de données, les fichiers de configuration, et les services web car la lisibilité de l’XML et sa prise en charge étendue en font un choix solide pour les données structurées.

## Comment faire :
En Go, utilisez le paquet `encoding/xml`. Analysons et générons de l’XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Les structures correspondent aux éléments XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Café"}
	coffee.Origin = []string{"Éthiopie", "Brésil"}

	// Marshaller la structure en XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Erreur : %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshaller l'XML en structure
	data := `
<plant id="27">
  <name>Café</name>
  <origin>Éthiopie</origin>
  <origin>Brésil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Erreur : %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
Sortie Exemple :
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Café</name>
   <origin>Éthiopie</origin>
   <origin>Brésil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Café Origin:[Éthiopie Brésil]}
```

## Exploration Approfondie
L’XML existe depuis la fin des années 90, conçu pour l’édition électronique à grande échelle mais rapidement adopté pour le web. Des alternatives comme le JSON sont apparues, vantées pour leur simplicité, mais la validation des documents par des schémas et des espaces de noms de l’XML reste puissante pour les documents complexes. En Go, `encoding/xml` gère la plupart des tâches, mais pour de gros documents ou le traitement de flux, envisagez `xml.NewDecoder` et `xml.NewEncoder` pour un contrôle de bas niveau et de meilleures performances.

## Voir Aussi
- Le paquet `encoding/xml` de Go : https://pkg.go.dev/encoding/xml
- Tutoriel XML : https://www.w3schools.com/xml/
- Blog Go sur l’XML : https://blog.golang.org/xml
- Comparaison entre JSON et XML : https://www.json.org/xml.html