---
title:                "Obtenir la date actuelle"
html_title:           "Gleam: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Obtenez la date actuelle avec Gleam
Bienvenue dans cet article sur l'obtention de la date actuelle en utilisant le langage de programmation Gleam. Si vous êtes un programmeur, vous savez probablement que la date est une donnée importante dans de nombreuses applications. Dans cet article, nous allons couvrir comment obtenir la date actuelle en utilisant Gleam, pourquoi les programmeurs le font et quelques informations supplémentaires sur cette tâche courante.

## Quoi & Pourquoi?
Obtenir la date actuelle est simplement le fait de savoir quel jour et quelle heure il est. En tant que programmeur, cela est important pour de nombreuses raisons, notamment pour suivre les événements, enregistrer des horodatages et afficher l'heure aux utilisateurs. En utilisant Gleam, vous pouvez facilement obtenir la date actuelle pour répondre à ces besoins.

## Comment faire:
Voici un exemple de code qui utilise la fonction `Time.now()` pour obtenir la date actuelle en utilisant Gleam:

```Gleam
let date = Time.now()
IO.println("La date actuelle est: " ++ date.to_string())
```
Le code ci-dessus utilise la fonction `Time.now()` pour récupérer la date actuelle et l'affiche en utilisant la fonction `println()` de la bibliothèque `IO`. Vous pouvez également personnaliser le format de la date en utilisant d'autres fonctions telles que `format()` ou `to_iso8601()`.

## Plongée en profondeur:
Si vous êtes curieux d'en savoir plus sur l'obtention de la date actuelle en programmation, voici quelques informations supplémentaires pour vous. Historiquement, les programmeurs devaient écrire leur propre code pour obtenir la date actuelle, mais avec des langages modernes tels que Gleam, il existe des fonctions prêtes à l'emploi pour cette tâche.

Il y a également d'autres façons d'obtenir la date actuelle, comme utiliser une API de fournisseur de services de temps ou récupérer la date du système d'exploitation de l'ordinateur. Cependant, en utilisant Gleam, vous pouvez obtenir la date actuelle sans avoir à dépendre d'une API externe ou d'un système d'exploitation.

## Voir aussi:
Si vous souhaitez en savoir plus sur l'utilisation de Gleam pour travailler avec la date et l'heure, voici quelques ressources utiles:

- Documentation Gleam: https://gleam.run
- Tutoriel Gleam sur la manipulation de la date et de l'heure: https://gleam.run/tutorials/getting-current-date-time
- Discussion sur le forum Gleam sur l'obtention de la date actuelle: https://gleam.run/community/t/getting-current-date
- Autres alternatives à la fonction `Time.now()` de Gleam: https://gleam.run/modules/time

Voilà, vous savez maintenant comment obtenir la date actuelle en utilisant Gleam. Nous espérons que cet article vous a été utile et n'hésitez pas à explorer les différentes façons d'obtenir la date et l'heure en utilisant ce langage de programmation moderne et performant. Bon codage!