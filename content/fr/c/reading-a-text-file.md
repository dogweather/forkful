---
title:    "C: Lecture d'un fichier texte"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

La lecture de fichiers texte est une opération courante en programmation C. En apprenant comment lire un fichier, vous pourrez développer des programmes plus robustes et polyvalents. De plus, cela vous permettra également de manipuler des données externes sans les avoir à entrer manuellement dans votre code.

# Comment faire

La première étape pour lire un fichier texte en C consiste à ouvrir le fichier à l'aide de la fonction `fopen()`. Cette fonction prend deux arguments : le nom du fichier et le mode de lecture. Voici un exemple de code :

```C
FILE* fichier = fopen("mon_fichier.txt", "r");
```

Une fois que le fichier est ouvert, vous pouvez lire son contenu en utilisant la fonction `fscanf()`. Cette fonction prend également deux arguments : le pointeur de fichier et le format de lecture. Voici un exemple de code pour lire le contenu du fichier ligne par ligne :

```C
char ligne[100];
while (fscanf(fichier, "%s", ligne) != EOF) {
    printf("%s", ligne);
}
```

Ensuite, vous pouvez manipuler et traiter les données lues selon vos besoins.

# Plongée en profondeur

Il est important de noter que lors de l'ouverture d'un fichier, il est important de vérifier si le fichier a bien été ouvert en vérifiant la valeur de retour de la fonction `fopen()`. Si la valeur de retour est un pointeur NULL, cela signifie que l'ouverture du fichier a échoué.

De plus, il est également important de fermer le fichier une fois que vous avez terminé de le lire en utilisant la fonction `fclose()`. Ceci garantit que toutes les modifications apportées au fichier sont enregistrées et que toutes les ressources utilisées par le fichier sont libérées.

# Voir aussi

- Tutoriel complet sur la lecture de fichiers en C : https://www.programiz.com/c-programming/c-file-input-output
- Documentation officielle de la fonction `fopen()` : http://www.cplusplus.com/reference/cstdio/fopen
- Vidéo explicative sur la lecture de fichiers en C : https://www.youtube.com/watch?v=FH2Wi3Z0-PI