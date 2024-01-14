---
title:    "Gleam: Lancer un nouveau projet"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Commencer un nouveau projet peut sembler intimidant, mais cela peut être une expérience enrichissante et gratifiante. Cela vous permet de mettre en pratique vos compétences de programmation, d'apprendre de nouvelles techniques et de créer quelque chose de nouveau et de passionnant.

## Comment faire

Pour ceux qui sont intéressés par la création de projets en utilisant Gleam, voici un guide étape par étape pour démarrer:

1. Tout d'abord, assurez-vous d'avoir installé Gleam sur votre ordinateur. Vous pouvez facilement le faire en suivant les instructions sur le site officiel de Gleam.

2. Une fois que Gleam est installé, créez un nouveau dossier pour votre projet. Dans ce dossier, initialisez un nouveau projet en utilisant la commande `gleam new nom_du_projet`.

3. Une fois que le projet est initialisé, vous pouvez commencer à coder en utilisant l'éditeur de votre choix. Gleam est un langage de programmation fonctionnel et optimisé pour le concurrence. Vous pouvez donc expérimenter avec des fonctions, des modules et des processus concurrents pour créer des applications performantes.

4. Vous pouvez également utiliser l'outil de compilation de Gleam pour générer le code JavaScript ou Erlang pour exécuter votre projet une fois terminé. Pour cela, utilisez la commande `gleam build` suivie du nom de votre projet.

Voici un exemple de code simple en Gleam qui affiche "Bonjour!" à l'écran:

```
Gleam
import gleam/pio

pub fn main() {
  pio.print("Bonjour!")
}
```

Et voici la sortie que vous devriez obtenir en exécutant ce code avec la commande `gleam run bonjour.gleam`:

```
Bonjour!
```

## Profondeur de plongée

Pour ceux qui souhaitent en savoir plus sur la création de projets avec Gleam, voici quelques conseils utiles:

- Utilisez l'outil de documentation intégré de Gleam en utilisant la commande `gleam docs` pour en savoir plus sur les fonctions et les types de données disponibles dans le langage.
- Rejoignez la communauté Gleam en ligne pour poser des questions et obtenir de l'aide de la part de développeurs expérimentés.
- Étudiez les exemples et les projets open source en utilisant Gleam pour voir comment les autres développeurs utilisent le langage.

En utilisant ces conseils, vous serez sur la bonne voie pour créer des projets étonnants en utilisant Gleam.

## Voir aussi

- Site officiel de Gleam: https://gleam.run/
- Tutoriels Gleam: https://gleam.run/book/
- Communauté Gleam en ligne: https://github.com/gleam-lang/gleam/discussions