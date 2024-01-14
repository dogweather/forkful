---
title:                "Gleam: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes développeur et que vous travaillez avec des données structurées, vous avez probablement entendu parler du format JSON. Il est largement utilisé pour stocker et échanger des données sur le web. Dans cet article, nous allons vous montrer pourquoi il vaut la peine de travailler avec JSON en utilisant le langage de programmation Gleam.

## Comment faire 

Pour commencer, nous allons créer un exemple simple de traitement de données JSON avec Gleam. Tout d'abord, nous avons besoin de déclarer un type de données pour représenter notre structure JSON :

```Gleam
type User {
    name : String,
    email : String,
    age : Int
}
```

Ensuite, nous pouvons utiliser la fonction `decode_json` pour transformer notre JSON en un type Gleam :

```Gleam
user_result : Decode.Result(User, Json.Decode.Error) =
    Json.decode_json(input)
```

Une fois le JSON transformé, nous pouvons accéder à ses propriétés en utilisant des fonctions de navigation :

```Gleam
case user_result {
    Ok(user) -> "User name: ${user.name}, email: ${user.email}, age: ${user.age} years old"
    Err(error) -> "Error decoding JSON: ${error}"
}
```

Enfin, voici un exemple de notre structure JSON et sa sortie correspondante :

Input (JSON) :

```Gleam
{
    "name": "John",
    "email": "john@example.com",
    "age": 30
}
```

Output :

```Gleam
"User name: John, email: john@example.com, age: 30 years old"
```

## Plongée en profondeur 

Maintenant que nous savons comment utiliser Gleam pour traiter des données JSON, jetons un coup d'œil à quelques fonctionnalités avancées.

### Validation de données JSON 

La validation des données est importante pour s'assurer que nous recevons les bonnes données et qu'elles sont dans le bon format. En utilisant les types de données et les fonctions de navigation de Gleam, nous pouvons facilement valider notre JSON et gérer les erreurs en cas de données manquantes ou incorrectes.

### Transformation de données JSON 

Il peut arriver que nous ayons besoin de transformer notre JSON en un autre format ou en modifier certaines parties. Avec Gleam, cela peut être fait en utilisant des fonctions de manipulation de données telles que `map` et `filter`.

## Voir aussi

- Documentation officielle de Gleam sur le traitement des données JSON : https://gleam.run/documentation/json
- Tutoriel pour débutants sur le traitement de données JSON avec Gleam : https://www.freecodecamp.org/news/how-to-work-with-json-in-gleam-programming-language/
- Exemples de projets utilisant Gleam pour traiter des données JSON : https://github.com/gleam-lang/awesome-gleam#json-processing