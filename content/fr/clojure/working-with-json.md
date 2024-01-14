---
title:                "Clojure: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi travailler avec JSON?

JSON (JavaScript Object Notation) est un format de données populaire utilisé pour échanger des informations sur le web. En tant que programmeur en Clojure, il est important d'être capable de travailler avec JSON afin d'intégrer des données provenant de différentes sources dans vos projets.

## Comment travailler avec JSON en Clojure

Pour commencer, vous aurez besoin d'importer la bibliothèque "cheshire" dans votre projet Clojure. Cette bibliothèque vous permettra de manipuler facilement des données JSON en utilisant des fonctions simples et intuitives.

```
(require '[cheshire.core :as json])
```

### Convertir des données Clojure en JSON

Pour convertir des données Clojure en JSON, vous pouvez utiliser la fonction "generate-string" de la bibliothèque Cheshire. Voici un exemple de données Clojure et le résultat JSON correspondant:

```Clojure
(def data {:nom "Jean" :age 25 :ville "Paris"})
(json/generate-string data)
```

Sortie:

```json
{"nom":"Jean","age":25,"ville":"Paris"}
```

### Convertir JSON en données Clojure

Pour convertir des données JSON en données Clojure, vous pouvez utiliser la fonction "parse-string" de la bibliothèque Cheshire. Voici un exemple de données JSON et le résultat correspondant en données Clojure:

```Clojure
(def json-data "{\"nom\":\"Marie\",\"age\":30,\"ville\":\"Lyon\"}")
(json/parse-string json-data true)
```

Sortie:

```Clojure
{:nom "Marie" :age 30 :ville "Lyon"}
```

## Plongée profonde dans JSON en Clojure

En plus des fonctions de base pour travailler avec JSON, la bibliothèque Cheshire offre également des fonctionnalités avancées telles que la validation de schéma, la manipulation de grands fichiers JSON et la personnalisation du format de sortie JSON. Pour en savoir plus sur ces fonctionnalités, vous pouvez consulter la documentation officielle de Cheshire.

## Voir aussi

- [Documentation officielle de Cheshire](https://github.com/dakrone/cheshire)
- [Tutoriel pour travailler avec JSON en Clojure](https://clojure.org/guides/json)
- [Blog post sur la manipulation de données JSON en Clojure](https://yogthos.net/posts/2014-08-18-working-with-json-in-clojure.html)