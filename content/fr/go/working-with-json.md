---
title:                "Travailler avec json"
html_title:           "Go: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-json.md"
---

{{< edit_this_page >}}

# JSON en Go : Pourquoi et Comment

## Qu'est-ce que le JSON et pourquoi les programmeurs l'utilisent-ils?

Le JSON (JavaScript Object Notation) est un format de données léger et facile à lire, souvent utilisé pour transférer des données sur le web. Les programmeurs l'utilisent car il est universellement pris en charge, facile à comprendre et à utiliser. Il est également compatible avec de nombreux langages de programmation, y compris Go.

## Comment faire ?

Pour travailler avec JSON en Go, la première étape est d'importer le package "encoding/json" dans votre code. Ensuite, vous pouvez utiliser la fonction "Unmarshal" pour convertir des données JSON en structures Go. Voici un exemple :

```Go
import "encoding/json"
jsonStr := `{"name": "John", "age": 25}`
type Person struct {
	Name string `json:"name"`
	Age int `json:"age"`
}
var p Person
err := json.Unmarshal([]byte(jsonStr), &p)
if err != nil {
    fmt.Println(err)
}
fmt.Println(p.Name, p.Age) // Output: John, 25
```

Pour convertir des structures Go en JSON, vous pouvez utiliser la fonction "Marshal". Voici un exemple :

```Go
type Person struct {
	Name string `json:"name"`
	Age int `json:"age"`
}
p := Person{"John", 25}
jsonBytes, err := json.Marshal(p)
if err != nil {
    fmt.Println(err)
}
fmt.Println(string(jsonBytes)) // Output: {"name":"John","age":25}
```

## Plongée en profondeur

JSON a été créé en 2001 par Douglas Crockford en tant que format de données léger pour JavaScript. Aujourd'hui, il est largement utilisé dans de nombreux domaines, tels que les applications mobiles, les services web et l'Internet des objets.

Bien qu'il soit souvent utilisé pour transférer des données, il existe d'autres formats de données tels que XML et YAML. JSON se distingue par sa simplicité et sa lisibilité.

En termes d'implémentation, le package "encoding/json" de Go utilise la réflexion pour encoder et décoder des structures en JSON. Cela signifie qu'il examine le type et les champs d'une structure et les mappe avec les données JSON.

## À voir aussi

Vous pouvez en apprendre davantage sur le package "encoding/json" de Go en consultant sa documentation officielle : https://golang.org/pkg/encoding/json/.

Si vous souhaitez en savoir plus sur JSON en général, vous pouvez consulter le site officiel de JSON : https://www.json.org/.