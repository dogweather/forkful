---
title:                "Travailler avec yaml"
html_title:           "Go: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde du développement logiciel, la gestion des données est essentielle pour créer des applications performantes. YAML est un langage de sérialisation de données très populaire qui est largement utilisé pour stocker et transmettre les données. En apprenant à travailler avec YAML à travers Go, vous serez en mesure de manipuler et de traiter des données de manière efficace et simple.

## Comment faire

Tout d'abord, vous devrez installer Go sur votre ordinateur et importer le package YAML en utilisant la commande suivante :

```Go 
import "gopkg.in/yaml.v3"
```

Ensuite, vous pouvez créer un fichier YAML en utilisant la syntaxe suivante :

```Go
file := map[string]interface{}{
    "nom": "Lucie",
    "âge": 26,
    "intérêts": []string{"programmation", "voyages", "vélo"},
}
```

Vous pouvez ensuite encoder ces données en YAML en utilisant la fonction `Marshal` et en spécifiant le fichier de sortie :

```Go
data, err := yaml.Marshal(file)
if err != nil {
    log.Fatalf("Impossible d'encoder les données YAML: %v", err)
}

err = ioutil.WriteFile("fichier.yaml", data, 0644)
if err != nil {
    log.Fatalf("Impossible de créer le fichier YAML: %v", err)
}
```

Pour décoder un fichier YAML existant, vous pouvez utiliser la fonction `Unmarshal` et spécifier un pointeur vers la structure de données cible :

```Go
data, err := ioutil.ReadFile("fichier.yaml")
if err != nil {
    log.Fatalf("Impossible de lire le fichier YAML: %v", err)
}

var newFile map[string]interface{}

err = yaml.Unmarshal(data, &newFile)
if err != nil {
    log.Fatalf("Impossible de décoder les données YAML: %v", err)
}
```

Enfin, vous pouvez également utiliser la fonction `Decode` pour lire un flux YAML en direct sans avoir à le stocker dans un fichier :

```Go
decoder := yaml.NewDecoder(os.StdIn)

var input map[string]interface{}
for {
    err := decoder.Decode(&input)
    if err == io.EOF {
        break
    }
    if err != nil {
        log.Fatalf("Impossible de lire le flux YAML: %v", err)
    }

    // faire quelque chose avec les données lues
}
```

## Plongée en profondeur

En plus des fonctions de base pour encoder et décoder des données en YAML, Go offre également une variété de fonctions pratiques pour travailler avec ce format. Vous pouvez utiliser `Get` pour récupérer une valeur spécifique à partir d'un fichier YAML, `Path` pour extraire des données en utilisant une syntaxe de chemin, `Merge` pour fusionner plusieurs fichiers YAML, et bien plus encore !

Comme vous pouvez le constater, travailler avec YAML à travers Go est simple et efficace, vous permettant de gérer et de manipuler des données de manière fluide.

## Voir aussi

- [Documentation officielle de Go](https://golang.org/)
- [Documentation officielle de YAML](https://yaml.org/)
- [Package Go YAML](https://pkg.go.dev/gopkg.in/yaml.v3)