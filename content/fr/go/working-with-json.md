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

## Pourquoi

Travailler avec JSON est devenu très courant dans le monde de la programmation, en particulier avec Go, grâce à sa simplicité et sa compatibilité avec les applications web et mobiles. En comprenant comment travailler avec JSON, vous serez en mesure de communiquer efficacement et d'échanger des données avec d'autres systèmes.

## Comment faire

Pour commencer, assurez-vous d'avoir les connaissances de base en programmation avec Go. Ensuite, voici comment récupérer et utiliser des données JSON dans votre code :

1. Importez le package `"encoding/json"` dans votre code.

```Go
import "encoding/json"
```

2. Définissez une structure qui correspond au format JSON que vous souhaitez utiliser.

```Go
type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}
```

3. Utilisez la fonction `json.Unmarshal` pour convertir les données JSON en une variable de type struct.

```Go
var data = []byte(`{"name": "John Doe", "age": 25}`)
var person Person
err := json.Unmarshal(data, &person)
```

4. Vous pouvez maintenant accéder aux différentes valeurs de votre structure en utilisant le nom des attributs.

```Go
fmt.Println(person.Name) // John Doe
fmt.Println(person.Age) // 25
```

## Plongée en profondeur

Il est important de noter que l'utilisation de `json.Unmarshal` nécessite que la structure ait des balises de champ (tags) `json` pour chaque attribut. Ces balises indiquent au package `encoding/json` comment mapper les données JSON aux champs de la structure. Si vous ne spécifiez pas ces balises, l'opération d'unmarshaling ne sera pas possible.

De plus, il est également possible de convertir un type struct en données JSON en utilisant la fonction `json.Marshal`.

```Go
person := Person{Name: "Jane Doe", Age: 30}
jsonData, err := json.Marshal(person)
fmt.Println(string(jsonData)) // {"name": "Jane Doe", "age": 30}
```

Pour en savoir plus sur les différentes possibilités de travailler avec JSON en utilisant Go, consultez la documentation officielle : https://golang.org/pkg/encoding/json/

## Voir aussi

Pour en savoir plus sur Go et son utilisation dans le développement d'applications, voici quelques liens utiles :

- Documentation officielle de Go : https://golang.org/doc/
- Tutoriels interactifs sur Go : https://tour.golang.org/welcome/1
- Comparaison entre Go et d'autres langages de programmation : https://golang.org/doc/faq#languages
- Exemples pratiques d'utilisation de Go : https://github.com/golang/example