---
title:                "Commencer un nouveau projet"
html_title:           "C: Commencer un nouveau projet"
simple_title:         "Commencer un nouveau projet"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes passionné par la programmation et que vous souhaitez constamment améliorer vos compétences, démarrer un nouveau projet est la meilleure façon de le faire. Cela vous permettra de vous mettre au défi et d'apprendre de nouvelles techniques et pratiques de codage.

## Comment faire

Pour commencer un nouveau projet en C, vous aurez besoin d'un éditeur de code tel que Visual Studio Code ou Code::Blocks. Assurez-vous d'installer un compilateur C tel que GCC ou Clang pour exécuter votre code.

Voici un exemple de code simple pour afficher "Bonjour, monde !" à l'écran :

```C
#include <stdio.h>

int main() {
  printf("Bonjour, monde !\n");
  return 0;
}
```

Lorsque vous exécutez ce code, vous devriez voir la phrase "Bonjour, monde !" s'afficher dans la console de votre éditeur de code.

Vous pouvez également créer une fonction pour demander à l'utilisateur son nom et lui souhaiter la bienvenue :

```C
#include <stdio.h>

void welcome() {
  char name[20];
  printf("Quel est votre nom ?\n");
  scanf("%s", name);
  printf("Bienvenue, %s !\n", name);
}

int main() {
  welcome();
  return 0;
}
```

La fonction scanf accepte l'entrée utilisateur et la stocke dans la variable name. La phrase "Bienvenue, [nom] !" sera alors imprimée à l'écran.

## Plongée en profondeur

Lorsque vous démarrez un nouveau projet en C, il est important de bien comprendre l'objectif du projet et de planifier sa structure et son architecture. Assurez-vous de décomposer votre code en fonctions logiques pour rendre votre code plus lisible et facile à maintenir.

Vous devriez également vous familiariser avec les fonctions et bibliothèques standard du langage C, telles que printf pour l'affichage et scanf pour la saisie utilisateur.

N'oubliez pas de toujours commenter votre code pour expliquer vos choix de conception et rendre votre code plus compréhensible pour vous et pour les autres programmeurs travaillant sur le projet.

## Voir aussi

- [Tutoriel C pour débutants](https://www.learn-c.org/)
- [Documentation officielle du langage C](https://en.cppreference.com/w/c)
- [Guide de style de codage C](https://stackoverflow.com/questions/228783/what-is-your-favorite-c-coding-style)