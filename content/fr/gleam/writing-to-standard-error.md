---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Gleam: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi on le fait ?

L'écriture vers le canal d'erreur standard est une pratique courante en programmation. Cela consiste à envoyer des messages d'erreur vers une sortie spécifique, plutôt que vers la sortie standard. Les programmeurs le font pour améliorer la lisibilité du code et faciliter le débogage des erreurs.

## Comment faire ?

```Gleam
// Exemple de code montrant l'utilisation de l'écriture vers le canal d'erreur standard
fn main() {
  // Création d'un message d'erreur
  let err_message = "Attention : erreur de syntaxe.";

  // Utilisation de la fonction `io::stderr` pour écrire le message vers le canal d'erreur standard
  Gleam.io.stderr(err_message);
}
```

Lorsque ce code sera exécuté, le message d'erreur sera affiché directement vers le canal d'erreur standard. Si vous exécutez le code depuis un terminal, vous pourrez le voir s'afficher en rouge.

## Plongée dans les détails

Historiquement, les programmeurs utilisaient une fonction appelée `printf` pour imprimer des messages d'erreur. Cependant, avec l'avènement des languages fonctionnels, une approche plus sophistiquée a été adoptée, appelée "traitement des erreurs basées sur les types". Cette méthode permet d'utiliser des types de données spécifiques pour représenter les erreurs plutôt que de simplement utiliser du texte brut.

### Alternatives
Il existe d'autres méthodes pour gérer les messages d'erreur en programmation, comme le lancement d'exceptions ou l'utilisation de variables globales pour stocker les erreurs. Cependant, écrire vers le canal d'erreur standard reste une méthode pratique et efficace pour gérer les erreurs.

### Détails d'implémentation
En utilisant la fonction `io::stderr`, le message d'erreur sera envoyé vers le canal d'erreur standard en utilisant la fonction système `fwrite` pour l'impression. L'utilisation d'un type spécifique pour représenter les erreurs permet également une meilleure gestion de l'encodage et des caractères spéciaux.

## À découvrir
Pour en savoir plus sur l'écriture vers le canal d'erreur standard en Gleam, consultez la documentation officielle (https://gleam.run/builtin/io/).

 N'hésitez pas à explorer d'autres sources en ligne pour en apprendre plus sur les différentes méthodes de gestion des erreurs en programmation.