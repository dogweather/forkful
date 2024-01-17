---
title:                "Lecture d'un fichier texte"
html_title:           "C: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Lire un fichier texte consiste à accéder et à extraire des données à partir d'un fichier texte stocké sur un ordinateur. Les programmeurs utilisent cette technique pour traiter de grandes quantités de données, telles que des bases de données ou des fichiers de configuration.

## Comment faire:
Voici un exemple simple de code qui lit un fichier texte en utilisant la fonction `fscanf()` :

```
C
#include <stdio.h>

int main() {
  FILE *file = fopen("fichier.txt", "r");
  int num;
  char str[50];
  while(fscanf(file, "%d %s", &num, str) != EOF) {
    printf("Numéro : %d, Mot : %s\n", num, str);
  }
  fclose(file);
  return 0;
}
```

Supposons que le contenu du fichier `fichier.txt` soit le suivant :
```
1 Bonjour
2 Monde
```

Le code ci-dessus affichera la sortie suivante :
```
Numéro : 1, Mot: Bonjour
Numéro : 2, Mot : Monde
```
Il est important de noter que le fichier doit être ouvert avec le mode "r" pour pouvoir être lu.

## Plongée en profondeur:
Lire des fichiers texte remonte aux premiers jours de la programmation informatique. C'était à l'époque où les ordinateurs stockaient les programmes et les données sur des cartes perforées. Aujourd'hui, il existe de nombreuses alternatives pour lire des fichiers de données, telles que les bases de données et les fichiers XML.

La fonction `fscanf()` elle-même peut être un peu délicate à utiliser, car elle peut être sensible aux erreurs de formatage du fichier. Une alternative courante est la fonction `fgets()`, qui peut être utilisée pour lire une ligne complète à la fois.

## Voir aussi:
- [Documentation de `fscanf()` sur cppreference.com](https://en.cppreference.com/w/c/io/fscanf)
- [Documentation de `fgets()` sur cppreference.com](https://en.cppreference.com/w/c/io/fgets)