---
title:                "Travailler avec yaml"
html_title:           "C++: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeurs en C++, vous avez probablement entendu parler de YAML. Mais vous vous demandez peut-être pourquoi vous devriez vous en soucier. Eh bien, YAML est un format de sérialisation de données qui est beaucoup plus facile à utiliser et à lire que le JSON ou le XML. Si vous manipulez souvent des données structurées dans vos applications, YAML peut être un outil très utile à ajouter à votre boîte à outils.

## Comment utiliser YAML en C++

Pour commencer à travailler avec YAML en C++, vous devrez d'abord inclure la bibliothèque YAML dans votre projet. Ensuite, vous pourrez utiliser la fonction "LoadFile" pour charger un fichier YAML et le stocker dans un objet "Node". Une fois que vous avez chargé le fichier, vous pouvez accéder aux données en utilisant la notation "[]" pour parcourir les différentes clés et valeurs.

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml"); // Charger le fichier YAML dans un objet "Node"
    std::cout << config["key"]["subkey"].as<std::string>(); // Accéder à la valeur de la sous-clé "subkey" du "key"
    return 0; 
}
```

### Exemple de fichier YAML

Voici un exemple de fichier YAML qui pourrait être chargé dans l'exemple de code ci-dessus :

```YAML
key:
    subkey: "valeur"
```

### Sortie de l'exemple de code

La sortie de l'exemple de code ci-dessus sera "valeur", car c'est la valeur associée à la clé "subkey" dans notre fichier YAML.

## Plongée en profondeur

Maintenant que vous avez une compréhension de base de comment utiliser YAML en C++, voici quelques informations supplémentaires pour approfondir votre compréhension :

- YAML utilise une syntaxe simple et facile à lire, qui le rend très populaire pour définir des configurations et des données structurées.
- Il existe plusieurs façons de charger des fichiers YAML, en fonction de vos besoins. Vous pouvez utiliser "LoadFile", "LoadAll", ou même charger manuellement chaque nœud et sa valeur.
- Les objets de la bibliothèque YAML sont hérités de la classe "YAML::Node", ce qui signifie que vous pouvez accéder aux données de différentes manières, en utilisant des opérateurs comme "[]" ou ".".

## Voir aussi

Maintenant que vous savez comment utiliser YAML en C++, voici quelques liens utiles pour continuer à apprendre et à approfondir vos connaissances :

- Documentation officielle de la bibliothèque YAML : https://github.com/jbeder/yaml-cpp/wiki/Tutorial
- Guide de référence rapide pour travailler avec YAML en C++ : https://www.codeproject.com/Articles/1169865/Working-with-YAML-using-Cplusplus
- Tutoriel vidéo pour apprendre à utiliser YAML en C++ : https://www.youtube.com/watch?v=Q2iyXQ5aVHk