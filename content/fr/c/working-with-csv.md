---
title:                "Travailler avec des fichiers CSV"
html_title:           "C: Travailler avec des fichiers CSV"
simple_title:         "Travailler avec des fichiers CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-csv.md"
---

{{< edit_this_page >}}

# Quoi et Pourquoi?
CSV (Comma-Separated Values) est un format de fichier couramment utilisÃ© pour stocker et Ã©changer des donnÃ©es tabulaires. Les programmeurs utilisent souvent CSV pour importer et exporter des donnÃ©es Ã partir de bases de donnÃ©es, de feuilles de calcul et d'autres applications. CSV est un format simple et universellement pris en charge, ce qui le rend idÃ©al pour la manipulation de donnÃ©es dans diverses applications de programmation.

# Comment faire:
Voici un exemple de code en C pour lire et afficher les donnÃ©es d'un fichier CSV:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    // Ouvrir le fichier CSV en mode lecture
    FILE* fichier_csv = fopen("donnees.csv", "r");

    // Lire et afficher chaque ligne du fichier
    char ligne[100];
    while (fgets(ligne, 100, fichier_csv) != NULL) {
        // SÃ©parer les donnÃ©es en utilisant la virgule comme dÃ©limiteur
        char* donnees = strtok(ligne, ",");
        while (donnees != NULL) {
            printf("%s ", donnees);
            donnees = strtok(NULL, ",");
        }
        printf("\n");
    }

    // Fermer le fichier
    fclose(fichier_csv);

    return 0;
}
```

Voici un exemple de donnÃ©es CSV dans le fichier "donnees.csv":
```
nom,prenom,age,ville
Dupont,Paul,25,Paris
Martin,Marie,30,Lyon
```

Et voici la sortie correspondante du programme:
```
nom prenom age ville
Dupont Paul 25 Paris
Martin Marie 30 Lyon
```

# Le bain Ã  remous:
CSV a Ã©tÃ© crÃ©Ã© dans les annÃ©es 1970 par des sociÃ©tÃ©s informatiques pour faciliter l'Ã©change de donnÃ©es entre diffÃ©rents systÃ¨mes. Il est Ã©galement souvent utilisÃ© pour migrer des donnÃ©es vers de nouveaux systÃ¨mes ou pour des opÃ©rations de sauvegarde. Bien qu'il soit souvent utilisÃ© pour une manipulation simple de donnÃ©es, il existe des bibliothÃ¨ques et des outils plus avancÃ©s pour manipuler les donnÃ©es CSV, tels que "libcsv" et "csvkit".

# Voir aussi:
- [Le format CSV sur Wikipedia](https://fr.wikipedia.org/wiki/Comma-separated_values)
- [Documentation sur les fonctions de manipulation de chaÃ®nes en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Csvkit: une suite d'outils en ligne de commande pour travailler avec les donnÃ©es CSV](https://csvkit.readthedocs.io/en/latest/)