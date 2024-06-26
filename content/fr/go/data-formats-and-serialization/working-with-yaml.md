---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:39.020135-07:00
description: "Comment faire : Pour travailler avec YAML en Go, vous devrez d'abord\
  \ importer une biblioth\xE8que qui prend en charge l'analyse et la s\xE9rialisation\
  \ de YAML,\u2026"
lastmod: '2024-03-13T22:44:57.157949-06:00'
model: gpt-4-0125-preview
summary: "Pour travailler avec YAML en Go, vous devrez d'abord importer une biblioth\xE8\
  que qui prend en charge l'analyse et la s\xE9rialisation de YAML, puisque la biblioth\xE8\
  que standard de Go n'inclut pas de support direct pour YAML."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Pour travailler avec YAML en Go, vous devrez d'abord importer une bibliothèque qui prend en charge l'analyse et la sérialisation de YAML, puisque la bibliothèque standard de Go n'inclut pas de support direct pour YAML. La bibliothèque la plus populaire à cette fin est "gopkg.in/yaml.v3". Voici comment commencer :

1. **Installation du package YAML :**

```bash
go get gopkg.in/yaml.v3
```

2. **Analyse de YAML dans une structure Go :**

D'abord, définissez une structure en Go qui correspond à la structure de vos données YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("erreur: %v", err)
  }
  fmt.Printf("Utilisateur: %s\nMot de passe: %s\n", config.Database.User, config.Database.Password)
}
```

**Exemple de sortie :**

```
Utilisateur: admin
Mot de passe: secret
```

3. **Sérialisation d'une structure Go en YAML :**

Voici comment convertir une structure Go de retour en YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("erreur: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Exemple de sortie :**

```yaml
---
database:
  user: admin
  password: supersecret
```

## Approfondissement :
L'utilisation de YAML dans le développement logiciel a augmenté en raison de son format lisible par l'homme, ce qui en fait un choix idéal pour les fichiers de configuration, la documentation ou les formats d'échange de données. Comparé à son homologue JSON, YAML offre des commentaires, des types scalaires et des fonctionnalités de relation, fournissant un cadre de sérialisation de données plus riche. Cependant, sa flexibilité et ses fonctionnalités ont pour contrepartie une complexité d'analyse, conduisant à des risques de sécurité potentiels lorsqu'il n'est pas manipulé avec soin (par exemple, exécution de code arbitraire).

La bibliothèque "gopkg.in/yaml.v3" pour Go est une solution robuste pour le traitement de YAML, trouvant un équilibre entre facilité d'utilisation et support complet des fonctionnalités. En l'état actuel, bien qu'il existe des alternatives comme "go-yaml/yaml" (la bibliothèque derrière "gopkg.in/yaml.v3"), la version choisie dépend généralement des exigences spécifiques du projet ou des préférences personnelles. Lorsqu'on travaille avec d'énormes ensembles de données ou des applications critiques en termes de performance, les programmeurs pourraient considérer des formats plus simples comme JSON pour leur temps d'analyse réduit et la surcharge de mémoire moindre. Néanmoins, pour les fichiers de configuration ou les paramètres où la lisibilité humaine et la facilité d'utilisation sont primordiales, YAML reste un concurrent solide dans l'écosystème Go.
