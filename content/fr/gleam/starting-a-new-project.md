---
title:    "Gleam: Commencer un nouveau projet"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un passionné de programmation et que vous cherchez un nouveau langage à apprendre, alors Gleam pourrait être la réponse. Avec sa syntaxe minimaliste et son support pour la programmation fonctionnelle et orientée objet, Gleam offre une expérience de développement moderne et robuste. De plus, sa compatibilité avec Erlang et sa gestion efficace de la concurrence en font un langage très prometteur pour les projets de toutes tailles.

## Comment faire

Pour commencer un nouveau projet en Gleam, il suffit de suivre ces quelques étapes :

```Gleam
// Importer les bibliothèques nécessaires
use orkestra/http
use gleam/json

// Définir une fonction qui effectue une requête HTTP
pub fn getInfo(url: String) String {
  // Faire la requête et stocker le résultat dans une variable
  let response = http.get(url)

  // Extraire les données JSON de la réponse
  let json = response.body()
    |> json.from_string

  // Récupérer une clé spécifique du JSON
  let info = json["info"]

  info
}
```

Ce code montre comment importer les bibliothèques nécessaires, effectuer une requête HTTP et extraire des données JSON de la réponse. Bien entendu, il est possible de créer des structures de données plus complexes et de les manipuler selon les besoins de votre projet.

## Plongée en profondeur

L'une des principales caractéristiques de Gleam est sa compilation en bytecode, qui lui permet d'être exécuté sur la machine virtuelle Erlang (BEAM). Cela signifie que les projets Gleam bénéficient de la fiabilité et de la concurrence fournies par Erlang, tout en utilisant une syntaxe plus moderne et plus accessible.

De plus, Gleam encourage le développement d'applications modulaires grâce à son système de modules et de types forts. Ainsi, il est plus facile de gérer la complexité de vos projets et de les faire évoluer au fil du temps.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/)
- [Exemples de projets Gleam](https://github.com/gleam-lang)
- [Communauté Gleam sur Discord](https://discord.gg/jDWWSDC)