---
title:                "Travailler avec yaml"
html_title:           "PHP: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# Qu'est-ce que YAML et pourquoi les programmeurs l'utilisent-ils?

YAML (Yet Another Markup Language) est un format de sérialisation de données populaire parmi les programmeurs, utilisé pour stocker et échanger des informations sous une forme lisible par l'homme. Il est couramment utilisé pour configurer des systèmes, stocker des données de configuration et échanger des informations entre applications.

Les programmeurs choisissent souvent YAML pour sa simplicité et sa lisibilité, ce qui en fait un choix populaire pour les projets open-source et les collaborateurs techniques.

## Comment utiliser YAML en PHP:

Voici un exemple simple de YAML en PHP:

```
<?php

// Charger la bibliothèque YAML
require 'vendor/autoload.php';

// Définir une variable contenant une chaîne YAML
$yaml = <<<YAML
greetings:
  - Salut
  - Bonjour
  - Hello
YAML;

// Analyser la chaîne de YAML en un tableau PHP
$greetings = Yaml::parse($yaml);

// Afficher le deuxième élément du tableau
echo $greetings['greetings'][1]; // Bonjour
```

Dans cet exemple, nous utilisons la bibliothèque PHP Symfony YAML pour charger, analyser et utiliser un fichier YAML. La méthode `parse()` convertit la chaîne YAML en un tableau PHP associatif.

## Plongez dans l'histoire de YAML, ses alternatives et ses détails d'implémentation:

YAML a été créé en 2001 par Clark Evans, Ingy döt Net et Oren Ben-Kiki. Son objectif était de créer un langage de sérialisation plus humain et plus facile à lire que les autres formats de données existants, tels que le XML et le JSON.

Bien qu'il y ait des alternatives à YAML comme JSON et XML, YAML est devenu populaire grâce à sa lisibilité et à sa simplicité. Cependant, il est important de noter que YAML peut également être plus lent à analyser que d'autres formats, ce qui peut être un facteur à prendre en compte dans des applications sensibles aux performances.

En termes d'implémentation, YAML peut être utilisé dans une variété de langages de programmation, et il existe de nombreuses bibliothèques et outils disponibles pour travailler avec le format.

## À voir également:

- [Site officiel de YAML](https://yaml.org/)
- [Documentation de l'extension YAML pour PHP](https://symfony.com/doc/current/components/yaml.html)
- [Comparaison de YAML avec JSON et XML](https://yaml.org/start.html#WhyUse?)
- [Exemples de YAML dans le monde réel](https://yaml.org/use.html)