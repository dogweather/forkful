---
title:                "C: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Qu'est-ce qui pousse les programmeurs à lire un fichier texte? La réponse est simple : les fichiers texte sont un outil de communication essentiel dans le monde de la programmation. Ils permettent de stocker et de transmettre des données de manière simple et efficace. Dans cet article, nous allons explorer comment lire un fichier texte en utilisant le langage de programmation C.

## Comment faire

Pour ouvrir et lire un fichier texte en C, nous utilisons la fonction `fopen` qui prend deux paramètres : le nom du fichier et le mode d'ouverture. Par exemple, pour ouvrir un fichier texte nommé "texte.txt" en mode lecture, nous utilisons la syntaxe suivante :

```C
FILE *fichier;
fichier = fopen("texte.txt", "r");
```

Ensuite, pour lire le contenu du fichier, nous utilisons la fonction `fscanf` qui fonctionne de la même manière que `scanf` pour la saisie au clavier :

```C
char texte[100];
fscanf(fichier, "%s", texte);
```

Il est important de noter que la fonction `fscanf` lit le contenu du fichier jusqu'à un espace blanc ou un retour à la ligne. Si vous voulez lire une ligne complète, utilisez plutôt la fonction `fgets`.

Une fois que vous avez terminé de lire le fichier, n'oubliez pas de le fermer en utilisant la fonction `fclose` :

```C
fclose(fichier);
```

## Plongée en profondeur

Maintenant que vous savez comment ouvrir et lire un fichier texte en utilisant C, voici quelques informations supplémentaires pour vous aider à maîtriser cette tâche. Tout d'abord, les modes d'ouverture les plus couramment utilisés sont `r` pour la lecture, `w` pour l'écriture et `a` pour ajouter du contenu au fichier existant. Deuxièmement, si vous voulez lire plusieurs mots du fichier en une seule fois, utilisez `%[^\n]` comme format dans la fonction `fscanf`.

Enfin, il est également possible de manipuler des fichiers texte en utilisant des fonctions de bas niveau telles que `fgetc` pour lire un seul caractère à la fois ou `fprintf` pour écrire dans un fichier. Mais cela dépasse le cadre de cet article.

## Voir aussi

Vous pouvez en apprendre davantage sur la manipulation de fichiers texte en C en consultant les ressources suivantes :

- [Documentation officielle de fopen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Guide de référence sur les fichiers en C](https://www.cprogramming.com/tutorial/cfileio.html)
- [Exemples de code pour lire et écrire dans un fichier en C](https://www.programiz.com/c-programming/file-input-output)