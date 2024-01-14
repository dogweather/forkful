---
title:    "Gleam: Écriture vers l'erreur standard"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en programmation?

L'écriture vers l'erreur standard est un concept essentiel en programmation où les erreurs et les messages de débogage sont envoyés vers un flux de sortie standard appelé stderr. Cela permet aux développeurs de mieux comprendre et de corriger les erreurs dans leur code, ce qui est crucial pour un développement efficace.

## Comment faire?

Ecrire vers l'erreur standard en Gleam est une tâche simple grâce à la fonction "std.error" qui prend en paramètre le message à afficher. Voici un exemple de code en Gleam pour écrire vers l'erreur standard:

```Gleam
import gleam/io

fn main() {
  gleam/io.std.error("Erreur: une variable non définie.")
}
```

Le résultat de l'exécution de ce code serait:

```
Erreur: une variable non définie.
```

Il est également possible de formater le message à afficher en utilisant la fonction "interpolation". Voici un autre exemple:

```Gleam
import gleam/io

fn main() {
  let name = "Maria"
  gleam/io.std.error(`Bonjour, ${name} !`)
}
```

Le résultat serait:

```
Bonjour, Maria !
```

## Plongée en profondeur

Ecrire vers l'erreur standard peut sembler simple, mais c'est en réalité un outil très utile pour le débogage de code. En utilisant cette technique, les développeurs peuvent obtenir des informations précieuses sur l'exécution de leur programme en temps réel, ce qui facilite la détection et la résolution des erreurs.

Il est également possible d'utiliser des bibliothèques externes comme "log" pour gérer et enregistrer les erreurs dans un fichier pour un suivi ultérieur. Cela peut être particulièrement utile pour les applications en production.

# Voir aussi

- [Documentation Gleam sur les fonctions standard pour interagir avec la console](https://gleam.run/documentation/stdlib/interacting-with-the-console)
- [Exemples de code pour écrire vers l'erreur standard en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/standard_error.gleam)