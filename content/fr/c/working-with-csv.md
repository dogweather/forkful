---
title:                "C: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-csv.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur en herbe, il est très probable que vous ayez entendu parler du format CSV (Comma-Separated Values). CSV est un type de fichier qui est très couramment utilisé pour stocker et échanger des données. Mais pourquoi est-il important de savoir comment travailler avec le format CSV en tant que programmeur ?

Eh bien, tout d'abord, CSV est un format de fichier très simple à comprendre et à manipuler. Même si vous n'êtes pas familier avec la programmation, vous pouvez facilement ouvrir un fichier CSV avec un simple éditeur de texte et lire les données. De plus, il est facile de faire correspondre les données stockées dans un fichier CSV avec une structure de données en mémoire, ce qui en fait un choix populaire pour stocker des données dans les programmes.

Donc, si vous voulez être un programmeur efficace et polyvalent, il est essentiel de comprendre comment travailler avec des fichiers CSV. Dans ce blog post, nous allons vous montrer comment le faire en utilisant le langage de programmation C.

# Comment faire

Pour lire un fichier CSV en langage C, nous utiliserons la bibliothèque standard `stdio.h`. Elle fournit des fonctions pour lire et écrire des fichiers dans différents formats, y compris le format CSV.

Tout d'abord, nous devons ouvrir le fichier CSV en utilisant la fonction `fopen()` et stocker le pointeur de fichier retourné dans une variable. Ensuite, nous pouvons utiliser la fonction `fscanf()` pour lire les données du fichier. Par exemple, si nous avons un fichier CSV avec trois colonnes contenant des valeurs entières, nous pouvons le lire comme ceci :

```C
#include <stdio.h>

int main() {
    // Ouverture du fichier CSV en mode lecture
    FILE *file = fopen("donnees.csv", "r");
    
    // Déclaration de variables pour stocker les valeurs lues
    int val1, val2, val3;
    
    // Lecture des valeurs du fichier
    fscanf(file, "%d,%d,%d", &val1, &val2, &val3);
    
    // Affichage des valeurs lues
    printf("Valeur 1 : %d\n", val1);
    printf("Valeur 2 : %d\n", val2);
    printf("Valeur 3 : %d\n", val3);
    
    // Fermeture du fichier
    fclose(file);
    
    return 0;
}
```

En exécutant ce code, nous obtiendrons en sortie :

```
Valeur 1 : 10
Valeur 2 : 20
Valeur 3 : 30
```

Cependant, si nous avons un fichier CSV avec plusieurs lignes de données, il devient plus compliqué de les lire à l'aide de `fscanf()`. Dans ce cas, il est préférable d'utiliser la fonction `fgets()` qui lit une ligne entière du fichier sous forme de chaîne de caractères. Nous pouvons ensuite utiliser la fonction `strtok()` pour découper la ligne en valeurs individuelles en utilisant le caractère de séparation (généralement une virgule pour les fichiers CSV). Voici un exemple de code utilisant ces fonctions :

```C
#include <stdio.h>
#include <string.h>

int main() {
    // Ouverture du fichier CSV en mode lecture
    FILE *file = fopen("donnees.csv", "r");
    
    // Déclaration de variables pour stocker les valeurs lues
    char line[100];
    char *token;
    
    // Lecture de chaque ligne du fichier
    while (fgets(line, sizeof(line), file)) {
        // Découpage de la ligne en utilisant le caractère de séparation ","
        token = strtok(line, ",");
        
        // Affichage de chaque valeur
        while (token != NULL) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }
    
    // Fermeture du fichier
    fclose(file);
    
    return 0;
}
```

En exécutant ce code, nous obtiendrons en sortie les valeurs de chaque cellule du fichier CSV.

# Plongée en profondeur

Comme mentionné précédemment, CSV est un format de fichier très simple à comprendre et à manipuler. Cependant, il a quelques subtilités qu'il est important de connaître pour travailler avec efficacement.

Tout d'abord, il est important de