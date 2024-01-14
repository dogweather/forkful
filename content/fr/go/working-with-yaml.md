---
title:                "Go: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-yaml.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur Go, vous avez probablement entendu parler de YAML. Mais pour ceux qui sont nouveaux dans le monde de la programmation, vous vous demandez peut-être pourquoi vous devriez vous intéresser à YAML. Eh bien, la réponse est simple : YAML est un langage simple et lisible par l'homme pour structurer les données et les configurer dans vos programmes Go. Il est utilisé pour créer des fichiers de configuration, des données JSON et même des scripts pour les pipelines CI/CD.

# Comment faire

La première étape pour travailler avec YAML en Go est d'importer le package "gopkg.in/yaml.v2". Ensuite, vous pouvez utiliser la méthode "Marshal" pour convertir une structure Go en données YAML et la méthode "Unmarshal" pour convertir des données YAML en une structure Go.

Voici un exemple de code pour écrire des données YAML :

```Go
import "gopkg.in/yaml.v2"

type Person struct {
	Name string
	Age  int
	City string
}

person := Person{"Jean", 30, "Paris"}

yamlData, err := yaml.Marshal(person)
if err != nil {
    log.Fatalf("Impossible de convertir en YAML: %v", err)
}

fmt.Printf(string(yamlData))
```

Cela produira l'output suivant :

```yaml
name: Jean
age: 30
city: Paris
```

Vous pouvez également utiliser YAML pour créer des fichiers de configuration pour votre application. Par exemple, vous pouvez créer un fichier "config.yaml" avec les paramètres de votre application :

```yaml
database:
  host: localhost
  port: 3306
  username: root
  password: secret
```

Ensuite, vous pouvez utiliser la méthode "Unmarshal" pour lire ces données dans une structure Go et les utiliser dans votre application.

# Plongée en profondeur

En plus de convertir des données en YAML et de les utiliser pour la configuration, vous pouvez également utiliser YAML pour créer des données JSON en utilisant la méthode "MarshalJSON". De plus, YAML prend en charge les types de données complexes tels que les tableaux et les structures imbriquées. Vous pouvez également utiliser des ancrages et des alias pour réutiliser des données dans un document YAML.

Une autre caractéristique intéressante de YAML est sa capacité à inclure des commentaires dans le document. Cela peut être utile pour documenter vos fichiers de configuration et rendre votre code plus facile à comprendre pour les autres développeurs.

# Voir aussi

- [Documentation officielle YAML en Go](https://godoc.org/gopkg.in/yaml.v2)
- [Exemples de projet utilisant YAML en Go](https://github.com/search?l=Go&q=yaml&type=Repositories)