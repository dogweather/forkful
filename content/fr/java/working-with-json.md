---
title:                "Java: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Java, vous avez probablement entendu parler de JSON. JSON, ou JavaScript Object Notation, est un format de données léger et populaire utilisé pour transférer des données sur le Web. Cela peut sembler intimidant pour certains, mais il est en fait assez simple à travailler avec JSON en utilisant Java. Dans cet article, nous allons explorer pourquoi il est utile d'utiliser JSON dans vos projets Java.

## Comment faire

Pour commencer à travailler avec JSON en Java, vous devrez d'abord inclure la bibliothèque JSON Java dans votre projet. Une fois que vous avez fait cela, vous pouvez suivre ces étapes pour lire et écrire des données JSON dans votre code :

```
// Importer la bibliothèque JSON
import org.json.JSONException;
import org.json.JSONObject;

// Définir une chaîne JSON de test
String jsonString = "{ \"nom\": \"Marie\", \"âge\": 25 }";

// Créer un objet JSONObject
JSONObject jsonObj = new JSONObject(jsonString);

// Récupérer des données de l'objet
String nom = jsonObj.getString("nom");
int age = jsonObj.getInt("âge");

// Afficher les données
System.out.println(nom + " a " + age + " ans.");
```

Ce code crée un objet JSON à partir d'une chaîne JSON de test, puis extrait les données de l'objet et les affiche. Vous pouvez également créer des objets JSON à partir de zéro et les enregistrer dans un fichier JSON en utilisant le même processus.

## Plongée en profondeur

JSON est un format de données facile à lire et à écrire pour les humains, mais il peut être un peu plus complexe pour les machines. C'est pourquoi il est important de comprendre comment lire et écrire des données JSON en Java. Le format JSON comprend des paires clé-valeur, où une clé est liée à une valeur. Il existe également des tableaux JSON, qui sont des listes ordonnées de valeurs. En utilisant les classes JSON fournies dans la bibliothèque Java, vous pouvez facilement accéder et manipuler ces données.

## Voir aussi

- [Documentation officielle de la bibliothèque JSON Java](https://github.com/stleary/JSON-java)
- [Tutoriel sur la lecture et l'écriture de fichiers JSON en Java](https://www.baeldung.com/java-json-file-read-write)
- [Introduction à JSON pour les développeurs Java](https://devopedia.org/json-java)