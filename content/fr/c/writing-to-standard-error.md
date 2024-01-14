---
title:    "C: Écrire vers l'erreur standard"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire dans la sortie d'erreur standard ?

Il y a plusieurs raisons pour lesquelles un programmeur peut choisir d'écrire dans la sortie d'erreur standard plutôt que dans la sortie standard. Cela peut être utile lors du débogage d'un programme, en fournissant des informations de débogage plus détaillées ou en signalant des erreurs spécifiques.

## Comment faire

Pour écrire dans la sortie d'erreur standard en utilisant le langage C, vous pouvez utiliser la fonction `fprintf()` en passant `stderr` comme premier argument. Par exemple :

```C
fprintf(stderr, "Ceci est une erreur !");
```
Cela écrira le message "Ceci est une erreur !" dans la sortie d'erreur standard.

## Plongée profonde

Il est important de noter que la sortie d'erreur standard est généralement associée à la console ou au terminal, plutôt qu'à un fichier. Cela signifie que les messages envoyés à la sortie d'erreur standard seront affichés directement à l'utilisateur.

De plus, les messages écrits dans la sortie d'erreur standard sont souvent affichés en rouge ou en jaune pour attirer l'attention de l'utilisateur et les distinguer des messages de sortie standard.

Il est également possible de rediriger la sortie d'erreur standard vers un fichier en utilisant des opérateurs de redirection en ligne de commande. Cela peut être utile pour enregistrer les erreurs dans un fichier plutôt que de les afficher à l'utilisateur.

## Voir aussi

- [Documentation officielle de fprintf en français](https://fr.cppreference.com/w/c/io/fprintf)
- [Tutoriel sur la gestion des erreurs en C](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c/15291-la-gestion-des-erreurs)
- [Différences entre la sortie standard et la sortie d'erreur standard en C](https://blog.hathix.com/differences-between-standard-output-and-standard-error-in-c)