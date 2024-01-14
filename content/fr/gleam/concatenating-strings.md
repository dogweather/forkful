---
title:    "Gleam: Concaténation de chaînes de caractères"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur en herbe ou expérimenté, vous savez peut-être déjà que travailler avec des chaînes de caractères est une partie importante de la programmation. La concaténation de chaînes de caractères, c'est-à-dire la fusion de plusieurs chaînes en une seule, peut sembler un concept simple, mais elle est en fait très utile dans de nombreux cas. Que ce soit pour la manipulation de données ou la création de textes dynamiques, la concaténation de chaînes de caractères est une compétence précieuse à avoir dans votre boîte à outils de développeur.

## Comment faire 

La concaténation de chaînes de caractères en Gleam peut sembler intimidante au premier abord, mais c'est en fait assez simple une fois que l'on comprend les bases. Voici un exemple de code pour vous montrer comment concaténer deux chaînes en une seule :

```Gleam
import gleam/strings

let prenom = "Jean"
let nom = "Dupont"

let nom_complet = strings.concat([prenom, " ", nom])
// la variable nom_complet contient maintenant "Jean Dupont"
```

Dans cet exemple, nous avons utilisé la fonction concat de la bibliothèque standard "gleam/strings" pour fusionner les chaînes "prenom" et "nom" en une seule chaîne "nom_complet". Vous pouvez également concaténer plus de deux chaînes en incluant simplement leurs valeurs dans le tableau fourni à la fonction concat.

## Deep Dive 

Maintenant que vous avez vu un exemple concret, vous vous demandez peut-être comment fonctionne réellement la concaténation de chaînes de caractères en Gleam. En fait, Gleam traite les chaînes de caractères comme des listes de caractères, et la concaténation fonctionne en combinant ces listes de caractères en une seule. C'est pourquoi la fonction concat prend un tableau de valeurs de type "String", plutôt qu'une simple chaîne. Ce système permet une plus grande flexibilité dans la concaténation et offre également de meilleures performances.

## Voir aussi 

- [Documentation officielle Gleam sur les chaînes de caractères](https://gleam.run/documentation/stdlib/strings/)
- [Tutoriel sur la manipulation des chaînes de caractères en Gleam](https://dev.to/timjrd/working-with-strings-in-gleam-2o7g)
- [Exemples de code Gleam sur la manipulation de chaînes de caractères](https://github.com/search?l=Gleam&o=desc&q=gleam+string&s=indexed&type=Code)