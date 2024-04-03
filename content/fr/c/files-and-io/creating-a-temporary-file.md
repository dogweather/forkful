---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:06.845096-07:00
description: "Cr\xE9er un fichier temporaire en C consiste \xE0 g\xE9n\xE9rer un fichier\
  \ destin\xE9 \xE0 \xEAtre utilis\xE9 pendant une courte dur\xE9e, g\xE9n\xE9ralement\
  \ comme espace de travail\u2026"
lastmod: '2024-03-13T22:44:58.393749-06:00'
model: gpt-4-0125-preview
summary: "Cr\xE9er un fichier temporaire en C consiste \xE0 g\xE9n\xE9rer un fichier\
  \ destin\xE9 \xE0 \xEAtre utilis\xE9 pendant une courte dur\xE9e, g\xE9n\xE9ralement\
  \ comme espace de travail temporaire pour le traitement ou le stockage de donn\xE9\
  es."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## Comment :
La création d’un fichier temporaire dans le langage de programmation C peut tirer parti de fonctions telles que `tmpfile()` et `mkstemp()`.

**Utilisation de `tmpfile()`** : Cette fonction crée un fichier temporaire unique qui est automatiquement supprimé lorsque le programme se termine ou que le fichier est fermé.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Échec de la création du fichier temporaire");
        return 1;
    }

    // Écriture de données dans le fichier temporaire
    fputs("Ceci est un test.\n", temp);

    // Retour au début et lecture de ce que nous avons écrit
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Suppression automatique à la fermeture ou à la sortie du programme
    fclose(temp);

    return 0;
}
```
**Sortie d'exemple :**
```
Ceci est un test.
```

**Utilisation de `mkstemp()`** : Offre plus de contrôle sur l'emplacement du fichier temporaire et ses autorisations. Il nécessite une chaîne de format qui se termine par `XXXXXX` qu'il remplace ensuite par une séquence unique pour éviter les collisions de noms.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mon_temp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Échec de la création du fichier temporaire");
        return 1;
    }
    
    printf("Fichier temporaire créé : %s\n", template);

    // Les fichiers temporaires créés avec mkstemp() doivent être supprimés manuellement
    unlink(template);

    close(fd);
    return 0;
}
```
**Sortie d'exemple :**
```
Fichier temporaire créé : /tmp/mon_temp-abc123
```

## Exploration Approfondie
Le concept de fichiers temporaires n'est pas propre au C mais est une fonctionnalité commune dans de nombreux environnements de programmation en raison de son utilité dans la gestion des données éphémères. La fonction `tmpfile()`, normalisée dans la norme ISO C, crée un fichier avec un nom unique dans un répertoire standard, mais son existence est éphémère, ce qui le rend idéal pour des opérations sécurisées ou temporaires.

Une limitation notable de `tmpfile()` est sa dépendance au répertoire temporaire par défaut, qui peut ne pas convenir à toutes les applications, notamment en termes de permissions ou de sécurité. En revanche, `mkstemp()` permet de spécifier le répertoire et garantit la création sécurisée de fichiers avec des noms de fichiers uniques garantis en modifiant la chaîne de modèle fournie, offrant une solution plus polyvalente au prix de la gestion manuelle des fichiers.

Cependant, la création de fichiers temporaires peut introduire des vulnérabilités de sécurité, telles que des conditions de concurrence, si elle n'est pas gérée correctement. Par exemple, `tmpfile()` et `mkstemp()` abordent différents aspects de la création sécurisée de fichiers temporaires (suppression automatique et génération de nom sécurisée, respectivement), mais aucune n'est une solution miracle. Les développeurs doivent prendre en compte les spécificités des besoins de sécurité de leur application, y compris les vulnérabilités potentielles introduites par les fichiers temporaires, et peuvent avoir besoin de mettre en œuvre des mesures de protection supplémentaires au-delà de ce que ces fonctions fournissent.

Dans le paysage plus large de la programmation, des alternatives telles que le stockage en mémoire (par exemple, l'utilisation de structures de données dynamiques ou de fichiers mappés en mémoire) pourraient offrir de meilleures performances ou une meilleure sécurité pour la gestion des données temporaires. Néanmoins, les fichiers temporaires physiques restent un outil crucial dans de nombreux scénarios, en particulier pour les grands ensembles de données ou lorsque la communication inter-processus est impliquée.
