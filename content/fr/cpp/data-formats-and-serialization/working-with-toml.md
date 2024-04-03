---
date: 2024-01-26 04:19:32.635709-07:00
description: "TOML (Tom's Obvious, Minimal Language, ou en fran\xE7ais, \"Langage\
  \ Minimal et \xC9vident de Tom\") est un format de s\xE9rialisation de donn\xE9\
  es facile \xE0 lire gr\xE2ce\u2026"
lastmod: '2024-03-13T22:44:58.193705-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language, ou en fran\xE7ais, \"Langage Minimal\
  \ et \xC9vident de Tom\") est un format de s\xE9rialisation de donn\xE9es facile\
  \ \xE0 lire gr\xE2ce \xE0 sa s\xE9mantique claire."
title: Travailler avec TOML
weight: 39
---

## Quoi & Pourquoi ?
TOML (Tom's Obvious, Minimal Language, ou en français, "Langage Minimal et Évident de Tom") est un format de sérialisation de données facile à lire grâce à sa sémantique claire. Les programmeurs utilisent TOML pour les fichiers de configuration car il trouve un équilibre entre la lisibilité par l'homme et la capacité d'analyse par la machine.

## Comment faire :
Pour travailler avec TOML en C++, vous aurez besoin d'une bibliothèque comme `toml++`. Voici un démarrage rapide :

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // Analyser TOML à partir d'un fichier
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // Accéder à une valeur
    std::string title = config["title"].value_or("Sans Titre");
    std::cout << "Titre : " << title << '\n';

    // Modifier et sauvegarder TOML
    config["title"] = "Nouveau Titre";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

Exemple de `config.toml` :
```toml
title = "Exemple"
```

Exemple de sortie :
```plaintext
Titre : Exemple
```

## Plongée Profonde
TOML a été créé par Tom Preston-Werner en 2013 en tant qu'alternative à YAML et JSON. Il est conçu pour être simple et explicite, principalement pour les fichiers de configuration. Contrairement au JSON, TOML se concentre sur l'absence d'ambiguïté, ce qui signifie qu'il est déterministe dans la manière dont le document est analysé.

Parmi les alternatives à TOML figurent YAML, qui est plus permissif quant à ce qui est autorisé, bien que parfois au prix de la prévisibilité. JSON, une autre alternative, est assez strict dans sa structure mais pas aussi convivial pour les configurations en raison de l'absence de commentaires et de sa syntaxe lourde en accolades.

En implémentation, `toml++` est une bibliothèque C++17 uniquement d'entêtes qui est conforme à la dernière spécification TOML. Elle fournit une interface de type DOM pour naviguer et manipuler les données TOML, ce qui la rend facile à intégrer dans des projets. La bibliothèque s'occupe de l'analyse, de la validation, et de la génération de sortie, vous permettant de lire et de définir des données TOML en utilisant des types C++.

## Voir Aussi
- Le dépôt GitHub de TOML : https://github.com/toml-lang/toml
- `toml++`, une bibliothèque C++ pour TOML : https://github.com/marzer/tomlplusplus
- La documentation officielle de TOML avec des explications détaillées sur le format : https://toml.io/fr/
