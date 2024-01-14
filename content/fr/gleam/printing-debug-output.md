---
title:    "Gleam: Impression de sortie de débogage"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Imprimer des sorties de débogage peut sembler être une tâche insignifiante dans le processus de développement, mais elle peut être très utile pour comprendre le comportement de votre code et trouver des erreurs. Cela peut vous faire gagner du temps en éliminant les conjectures et en vous permettant de vous concentrer sur les parties réellement problématiques de votre code.

## Comment faire

Dans Gleam, il existe deux façons de printer des sorties de débogage : en utilisant la fonction `debug` ou en imprimant directement avec `io.println`. Voici un exemple de chaque méthode :

```
// Utilisation de la fonction `debug` pour imprimer une valeur
gleam_debug:d