---
title:                "C: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

Écrire à la sortie d'erreur standard peut sembler être une tâche inutile pour certains, mais c'est en fait un aspect très important de la programmation en C. Cet article explorera pourquoi il est nécessaire d'écrire à la sortie d'erreur standard et comment le faire correctement.

# Comment faire

Écrire à la sortie d'erreur standard est très simple en C. Tout d'abord, il est important d'inclure la bibliothèque <stdio.h> qui contient la fonction "fprintf" que nous utiliserons pour écrire à la sortie d'erreur.

Dans l'exemple ci-dessous, nous utilisons "fprintf" pour écrire un message d'erreur personnalisé à la sortie d'erreur standard :

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Erreur : opération invalide\n");
  return 0;
}
```

Lorsque nous exécutons ce code, le message "Erreur : opération invalide" sera écrit à la sortie d'erreur standard. Cela peut être utile pour informer les utilisateurs en cas d'erreurs ou pour faciliter le débogage.

# Plongée en profondeur

Écrire à la sortie d'erreur standard est particulièrement important dans les programmes qui nécessitent une entrée utilisateur. Si une erreur se produit, le programme ne doit pas simplement se terminer sans informer l'utilisateur de la nature de l'erreur. En utilisant la fonction "fprintf" à la sortie d'erreur, nous pouvons fournir des informations spécifiques sur l'erreur, ce qui peut aider à identifier et à résoudre le problème plus facilement.

Il est également recommandé d'utiliser la sortie d'erreur standard pour les messages de débogage. Au lieu d'afficher des messages à la sortie standard qui peuvent déranger l'interface utilisateur, les messages de débogage peuvent être envoyés à la sortie d'erreur standard pour les rendre plus discrets.

# Voir aussi

- [Tutoriel de C pour débutants](https://en.wikipedia.org/wiki/C_(programming_language))
- [Documentation officielle de la bibliothèque standard de C](https://en.cppreference.com/w/c)
- [Guide de débogage en C](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)