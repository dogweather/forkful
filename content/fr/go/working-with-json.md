---
title:                "Go: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-json.md"
---

{{< edit_this_page >}}

# Why

JSON (JavaScript Object Notation) est un format de données léger et largement utilisé pour échanger des informations entre les applications. Il est simple à comprendre et à utiliser, ce qui en fait un choix populaire pour le transfert de données entre les applications. Si vous programmez en Go, vous aurez certainement besoin de travailler avec JSON à un moment donné.

# Comment faire

Pour travailler avec JSON en Go, il est recommandé d'utiliser le package standard `encoding/json`. Voici les étapes à suivre pour travailler avec JSON dans votre code Go :

1. Importez le package `encoding/json` dans votre fichier Go :

```Go
import "encoding/json"
```

2. Définissez une structure Go pour représenter vos données JSON. Lors de la définition de cette structure, assurez-vous que les noms des champs correspondent exactement aux noms des clés dans votre JSON. Par exemple :

```Go
type User struct {
    Name string `json:"name"`
    Age int `json:"age"`
    Email string `json:"email"`
}
```

3. Utilisez la fonction `json.Marshal()` pour encoder votre structure en JSON :

```Go
user := User {
    Name: "John",
    Age: 25,
    Email: "john@example.com",
}
userJSON, err := json.Marshal(user)
```

4. Pour décoder du JSON en une structure Go, utilisez la fonction `json.Unmarshal()` :

```Go
jsonStr := `{"name": "John", "age": 25, "email": "john@example.com"}`
var user User
err := json.Unmarshal([]byte(jsonStr), &user)
```

# Plongée en profondeur

En plus des étapes de base mentionnées ci-dessus, voici quelques points importants à garder à l'esprit lors de la manipulation de JSON en Go :

- Vous pouvez personnaliser la structure de votre JSON en utilisant des balises structurales dans votre structure Go, comme nous l'avons fait avec les tags `json` dans l'exemple ci-dessus.
- Pour ignorer un champ lors de la sérialisation en JSON, utilisez le tag struct `json:"-"`
- Si votre champ est un tableau ou une slice, utilisez un pointeur pour empêcher Go de créer une nouvelle copie lors de la sérialisation.
- Vous pouvez utiliser la méthode `json.Unmarshal()` avec une interface vide (`interface{}`) pour décoder un JSON inconnu.
- Vous pouvez également utiliser des alias de type personnalisés lors de la manipulation de JSON en Go. Par exemple, vous pouvez définir `type UserID int` pour représenter un identifiant utilisateur en tant que nombre entier.

Pour en savoir plus sur la manipulation de JSON en Go, vous pouvez consulter la documentation officielle de Go sur `encoding/json` et essayer les exemples de code inclus dans le package.

# Voir aussi

- [Documentation officielle de Go sur l'encodage JSON](https://golang.org/pkg/encoding/json/)
- [Exemples de code pour le package `encoding/json` en Go](https://golang.org/src/encoding/json/example_test.go)
- [Comment utiliser des tags struct pour personnaliser l'encodage JSON en Go](https://www.digitalocean.com/community/tutorials/comprendre-et-utiliser-les-tags-struct-en-go-fr)