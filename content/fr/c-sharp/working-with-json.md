---
title:                "Travailler avec json"
html_title:           "C#: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?


Travailler avec JSON (JavaScript Object Notation) est une tâche courante pour les programmeurs en C#. C'est une façon de stocker et d'échanger des données dans un format léger et facile à utiliser. Les programmeurs utilisent JSON car il est universellement pris en charge et convient bien aux échanges de données entre différentes applications et systèmes.

## Comment faire:

Voici comment manipuler des données JSON en utilisant C#:

```
// Créer un objet JSON vide

var jsonObject = new JObject();

// Ajouter des propriétés à l'objet JSON

jsonObject.Add("nom", "Jean");
jsonObject.Add("âge", 25);
jsonObject.Add("ville", "Paris");

// Convertir l'objet JSON en chaîne de caractères

string jsonString = jsonObject.ToString();

// Afficher la chaîne de caractères JSON

Console.WriteLine(jsonString);

// Résultat: {"nom": "Jean", "âge": 25, "ville": "Paris"}
```

## Plongée en profondeur:

JSON a été développé dans les années 2000 pour remplacer XML dans les échanges de données en JavaScript. Depuis lors, il est devenu un format populaire pour le stockage et l'échange de données dans de nombreux langages de programmation, y compris C#. Les alternatives à JSON incluent XML, CSV et YAML. Les développeurs peuvent utiliser des bibliothèques tierces pour travailler avec JSON en C#, telles que Newtonsoft.Json ou System.Text.Json.

## Voir aussi:

- Site officiel de JSON: https://www.json.org/json-en.html
- Documentation de Microsoft sur la manipulation de JSON en C#: https://docs.microsoft.com/en-us/dotnet/standard/serialization/system-text-json-how-to?pivots=dotnet-5-0
- Bibliothèque Newtonsoft.Json: https://www.newtonsoft.com/json
- Bibliothèque System.Text.Json de Microsoft: https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=net-5.0