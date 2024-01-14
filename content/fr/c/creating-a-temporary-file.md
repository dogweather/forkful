---
title:    "C: Création d'un fichier temporaire"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi: 
La création de fichiers temporaires est un élément essentiel de la programmation en C. Elle permet de stocker temporairement des données dont on a besoin pour une tâche spécifique. Cela peut être particulièrement utile lors de la manipulation de fichiers volumineux ou lors de la gestion de données sensibles.

## Comment Faire: 
Voici un exemple de code simple qui montre comment créer un fichier temporaire en utilisant la fonction `fopen()` en C:

```C
FILE *fptr;
fptr = fopen("tempfile.txt", "w");
if (fptr == NULL)
{
    printf("Erreur lors de la création du fichier temporaire");
}
else
{
    printf("Fichier temporaire créé avec succès");
    // Effectuer les opérations nécessaires sur le fichier temporaire
    fclose(fptr); // Fermer le fichier une fois les opérations terminées
}
```

Le code ci-dessus ouvre un fichier temporaire avec le nom `tempfile.txt` et le mode `w` pour écrire des données. Il vérifie également si le fichier a été créé avec succès en utilisant la condition `if...else`. Une fois que les opérations nécessaires sur le fichier temporaire ont été effectuées, il est important de fermer le fichier à l'aide de la fonction `fclose()` pour libérer les ressources utilisées.

## Plongez en profondeur: 
La création de fichiers temporaires en C peut également être réalisée en utilisant la fonction `mkstemp()`. Elle fonctionne un peu différemment de `fopen()` car elle permet de générer un nom de fichier temporaire unique chaque fois qu'elle est appelée. Cela peut être utile pour éviter tout risque de conflit de noms de fichiers.

Voici un exemple de code utilisant `mkstemp()`:

```C
char template[] = "tempXXXXXX";
int fd = mkstemp(template);
if (fd == -1)
{
    printf("Erreur lors de la création du fichier temporaire");
}
else
{
    printf("Fichier temporaire créé avec succès");
    // Effectuer les opérations nécessaires sur le fichier temporaire
    close(fd); // Fermer le fichier une fois les opérations terminées
}
```

Ici, le nom du fichier temporaire est généré en utilisant la chaîne `template` contenant des caractères `X`. Lorsque `mkstemp()` est appelée, elle remplace ces `X` par des caractères aléatoires pour créer un nom de fichier unique. Comme pour `fopen()`, le fichier doit être fermé à l'aide de la fonction `close()` après utilisation.

## Voir aussi:
- [Documentation officielle de la fonction fopen en C](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Documentation officielle de la fonction mkstemp en C](https://www.cplusplus.com/reference/cstdlib/mkstemp/)
- [Article détaillé sur la création de fichiers temporaires en C](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)