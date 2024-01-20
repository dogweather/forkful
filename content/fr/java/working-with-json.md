---
title:                "Travailler avec JSON"
html_title:           "Java: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-json.md"
---

{{< edit_this_page >}}

## Que & Pourquoi?

Le JSON (JavaScript Object Notation) est un format de données léger et facile à lire pour stocker et échanger des informations entre différentes applications. Les programmeurs travaillent souvent avec JSON car il est largement utilisé dans le développement web pour échanger des données entre un serveur et un client.

## Comment faire:

Voici un exemple simple de création d'un objet JSON en Java:

```
JsonObject jsonObject = new JsonObject();
jsonObject.addProperty("nom", "Jean");
jsonObject.addProperty("age", 25);
jsonObject.addProperty("ville", "Paris");
System.out.println(jsonObject.toString());
```

Le code ci-dessus crée un objet JSON avec trois propriétés: nom, âge et ville, et imprime le résultat. Le résultat sera le suivant:
```
{"nom": "Jean", "age": 25, "ville": "Paris"}
```

Pour lire et manipuler des données JSON, vous pouvez utiliser la librairie GSON en ajoutant la dépendance à votre projet. Voici un exemple de lecture de données JSON à partir d'une URL:
```
URL url = new URL("https://example.com/api/data");
InputStream is = url.openStream();
BufferedReader rd = new BufferedReader(new InputStreamReader(is, Charset.forName("UTF-8")));
String jsonText = readAll(rd);
JSONObject json = new JSONObject(jsonText);
String nom = json.getString("nom"); //récupère la valeur "nom" du JSON
int age = json.getInt("age"); //récupère la valeur "age" du JSON
```

## Plongée en profondeur:

Le JSON a été inventé par Douglas Crockford en 2001 et est dérivé de la syntaxe d'objets de JavaScript. Il est largement utilisé dans les API REST pour transmettre des données entre un serveur et un client. D'autres alternatives pour le formatage des données sont XML et CSV.

La librairie GSON a été développée par Google pour faciliter la manipulation de données JSON en Java. Elle offre des fonctionnalités telles que la lecture et l'écriture d'objets Java en JSON et la validation des données JSON.

## Voir aussi:

- [Documentation de GSON](https://github.com/google/gson)
- [Documentation officielle sur le format JSON](https://www.json.org/json-fr.html)