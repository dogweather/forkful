---
title:                "C#: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi Utiliser JSON dans vos Programmes?

JSON, ou JavaScript Object Notation, est un format de données léger et facile à lire et à écrire. Il est devenu un choix populaire pour échanger des données entre différentes applications, en particulier dans les applications web et mobiles. En utilisant JSON, vous pouvez stocker et transmettre des données structurées de manière efficace et fiable.

## Comment Utiliser JSON en C#?

Pour commencer à utiliser JSON dans vos programmes C#, vous devez d'abord inclure la bibliothèque Newtonsoft.Json dans votre projet. Vous pouvez le faire en utilisant l'interface utilisateur ou en installant le package à partir de la console Gestionnaire de Package NuGet.

Une fois que vous avez ajouté la bibliothèque, vous pouvez commencer à convertir des objets en JSON et vice versa en utilisant les méthodes ```JsonConvert.SerializeObject()``` et ```JsonConvert.DeserializeObject()``` respectivement.

Voici un exemple de code qui utilise ces méthodes pour convertir un objet en JSON:

```C#
// Créez un objet de type Personne
Personne p = new Personne { Nom = "Jean", Age = 30 };

// Convertissez l'objet en JSON
string json = JsonConvert.SerializeObject(p);

// Affichez le résultat
Console.WriteLine(json);

// Résultat: {"Nom":"Jean","Age":30}
```

Vous pouvez également utiliser des annotations de données pour contrôler la sérialisation et la désérialisation de vos objets. Des exemples d'annotations peuvent être trouvés dans la documentation officielle de Newtonsoft.Json.

Pour convertir du JSON en un objet, il suffit d'utiliser ```JsonConvert.DeserializeObject()``` et de fournir le type d'objet souhaité en paramètre. Voici un exemple:

```C#
// JSON à désérialiser
string json = "{ Nom: 'Pierre', Age: 25 }";

// Convertir en objet Personne
Personne p = JsonConvert.DeserializeObject<Personne>(json);

// Affichez les propriétés de l'objet
Console.WriteLine($"Nom: {p.Nom}, Age: {p.Age}");

// Résultat: Nom: Pierre, Age: 25
```

## Plongée Profonde dans l'Utilisation de JSON

Il existe plusieurs autres fonctionnalités et options pour travailler avec JSON dans vos programmes C#. Vous pouvez, par exemple, utiliser des types anonymes pour éviter de créer des classes pour des objets qui ne seront utilisés que pour la sérialisation et la désérialisation.

De plus, Newtonsoft.Json offre également un support pour travailler avec des paramètres de serializeur personnalisés, des schémas JSON, et même la compression de données.

Pour en savoir plus sur toutes ces fonctionnalités et sur la manière de les utiliser, je vous recommande de consulter la documentation officielle de la bibliothèque.

## Voir Aussi

- Documentation officielle de Newtonsoft.Json: https://www.newtonsoft.com/json/help/html/Introduction.htm
- Tutoriel sur l'utilisation de JSON en C#: https://www.tutorialspoint.com/json/json_c_sharp_example.htm
- Article sur l'importance de JSON dans le développement web: https://javascript.plainenglish.io/why-json-matters-in-web-development-65dad00eef3c