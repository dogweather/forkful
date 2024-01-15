---
title:                "Travailler avec des fichiers csv"
html_title:           "C: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous intéresser au CSV en tant que programmeur en C. Eh bien, le CSV (Comma Separated Values) est un format de fichier couramment utilisé pour stocker des données tabulaires telles que des feuilles de calcul. Maîtriser le CSV dans votre code C vous permettra de manipuler facilement et efficacement les données tabulaires.

## Comment faire

Le CSV peut être facilement manipulé en utilisant des fonctions de la bibliothèque standard de C telles que `fopen()` pour ouvrir un fichier CSV, `fscanf()` pour lire les données et `fclose()` pour fermer le fichier. Voici un exemple de code pour lire un fichier CSV et afficher les données:

```C
#include <stdio.h>

int main() {
    // Ouvrir le fichier CSV
    FILE* fichier = fopen("donnees.csv", "r");

    // Créer des variables pour stocker les données lues
    int age;
    char nom[20];

    // Lire les données du fichier CSV
    fscanf(fichier, "%d,%s", &age, nom);

    // Afficher les données lues
    printf("Nom: %s\nAge: %d ans", nom, age);

    // Fermer le fichier CSV
    fclose(fichier);

    return 0;
}
```

### Output:

```
Nom: Jean
Age: 25 ans
```

Vous pouvez également utiliser les fonctions `fprintf()` et `fputc()` pour écrire des données dans un fichier CSV. Voici un exemple de code pour écrire des données dans un fichier CSV:

```C
#include <stdio.h>

int main() {
    // Ouvrir le fichier CSV en mode écriture
    FILE* fichier = fopen("nouvelles_donnees.csv", "w");

    // Ecrire des données dans le fichier au format CSV
    int age = 25;
    char nom[20] = "Marie";
    fprintf(fichier, "%d,%s", age, nom);

    // Fermer le fichier CSV
    fclose(fichier);

    return 0;
}
```

### Output (dans le fichier nouvelles_donnees.csv):

```
25,Marie
```

## Plongée en profondeur

Si vous souhaitez travailler avec des fichiers CSV plus complexes, vous pouvez également utiliser la bibliothèque `libcsv` qui offre des fonctions avancées pour la manipulation de données CSV. Vous pouvez l'installer en utilisant `apt-get` ou `yum` selon votre distribution Linux.

Voici un exemple de code pour lire un fichier CSV avec `libcsv`:

```C
#include <stdio.h>
#include <csv.h>

int main() {
    // Ouvrir le fichier CSV
    FILE* fichier = fopen("donnees.csv", "r");

    // Initialiser la structure de données CSV
    csv_parser* parser = csv_init(CSV_COL_DELIM, CSV_QUOTE_NONE, 0, NULL);

    // Lire les données du fichier CSV
    csv_parse_file(parser, fichier, NULL, 0, NULL, NULL, NULL);

    // Afficher les données lues
    int row_count = csv_get_row_count(parser);
    int column_count = csv_get_col_count(parser);
    for (int i = 0; i < row_count; i++) {
        for (int j = 0; j < column_count; j++) {
            printf("%s ", csv_get_cell_content(parser, i, j));
        }
        printf("\n");
    }

    // Libérer la mémoire et fermer le fichier CSV
    csv_free(parser);
    fclose(fichier);

    return 0;
}
```

### Output:

```
Nom Age
Jean 25
Marie 30
```

Voir aussi

- [Documentation de la bibliothèque libcsv](https://www.systutorials.com/docs/linux/man/3-csv_init/)
- [Tutoriel sur le traitement de fichiers CSV en C](https://www.thinkprogramming.co.uk/c-tutorial-processing-csv-files/)
- [Le format CSV sur Wikipedia](https://fr.wikipedia.org/wiki/Comma-separated_values)