---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:38.327069-07:00
description: "Dans le domaine de la programmation, travailler avec des fichiers CSV\
  \ (Valeurs S\xE9par\xE9es par des Virgules) implique de lire et d'\xE9crire des\
  \ donn\xE9es dans\u2026"
lastmod: '2024-03-11T00:14:32.268705-06:00'
model: gpt-4-0125-preview
summary: "Dans le domaine de la programmation, travailler avec des fichiers CSV (Valeurs\
  \ S\xE9par\xE9es par des Virgules) implique de lire et d'\xE9crire des donn\xE9\
  es dans\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Dans le domaine de la programmation, travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique de lire et d'écrire des données dans des fichiers texte organisés par lignes, où chaque ligne représente un enregistrement et les champs de chaque enregistrement sont séparés par des virgules. Les programmeurs manipulent des fichiers CSV pour faciliter l'import/export de données à travers divers systèmes, en raison de leur prise en charge répandue et de leur simplicité pour le stockage de données tabulaires.

## Comment faire :

### Lire des fichiers CSV
Pour lire un fichier CSV en C, nous utilisons des fonctions d'I/O de fichiers standards ainsi que des fonctions de manipulation de chaînes de caractères pour analyser chaque ligne. Voici un exemple basique de lecture d'un fichier CSV et d'affichage des champs de chaque ligne dans la console.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Impossible d'ouvrir le fichier\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while (field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Exemple de `data.csv` :
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

Sortie d'exemple :
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### Écrire dans des fichiers CSV
De manière similaire, écrire dans un fichier CSV implique l'utilisation de `fprintf` pour sauvegarder les données dans un format séparé par des virgules.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Impossible d'ouvrir le fichier\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

Contenu de l'exemple `output.csv` :
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## Approfondissement

Le format CSV, bien qu'apparemment simple, comporte ses nuances, comme la gestion des virgules dans les champs et l'encapsulation des champs avec des guillemets. Les exemples rudimentaires présentés ne tiennent pas compte de ces complexités, ni ne gèrent les erreurs potentielles de manière robuste.

Historiquement, la gestion des CSV en C a été largement manuelle en raison de la nature bas-niveau du langage et de l'absence d'abstractions de haut niveau intégrées pour de telles tâches. Cette gestion manuelle inclut l'ouverture de fichiers, la lecture de lignes, le fractionnement de chaînes et la conversion des types de données au besoin.

Bien que la manipulation directe de fichiers CSV en C offre des expériences d'apprentissage précieuses sur l'I/O de fichiers et la manipulation de chaînes, plusieurs alternatives modernes promettent une efficacité et des processus moins sujets aux erreurs. Des bibliothèques comme `libcsv` et `csv-parser` offrent des fonctions complètes pour la lecture et l'écriture de fichiers CSV, y compris le support pour les champs entre guillemets et les délimiteurs personnalisés.

Alternativement, lorsqu'on travaille dans des écosystèmes qui le supportent, l'intégration avec des langages ou des plateformes qui fournissent des fonctions de manipulation de CSV de haut niveau (comme Python avec sa bibliothèque `pandas`) peut être une route plus productive pour les applications nécessitant un traitement intensif des CSV. Cette approche inter-langage tire profit de la performance de C et des capacités de programmation système tout en utilisant la facilité d'utilisation d'autres langages pour des tâches spécifiques telles que la gestion des CSV.
