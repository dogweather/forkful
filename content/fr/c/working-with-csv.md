---
title:                "Manipulation de fichiers CSV"
date:                  2024-01-19
html_title:           "C: Manipulation de fichiers CSV"
simple_title:         "Manipulation de fichiers CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Manipuler les fichiers CSV (valeurs séparées par des virgules) c'est interagir avec des données tabulaires simples. Les programmeurs le font pour la simplicité d’intégration de ces données dans des systèmes divers.

## How to: (Comment faire :)
Voici comment lire et écrire des fichiers CSV en C.

### Lire un fichier CSV:
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fichier = fopen("exemple.csv", "r");
    char ligne[1024];
    
    while (fgets(ligne, 1024, fichier)) {
        printf("%s", ligne); // Affiche chaque ligne du fichier
    }
    
    fclose(fichier);
    return 0;
}
```

### Écrire dans un fichier CSV:
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fichier = fopen("sortie.csv", "w");
    fprintf(fichier, "nom,age\n"); // En-tête du CSV
    fprintf(fichier, "Alice,30\n");
    fprintf(fichier, "Bob,25\n");
    
    fclose(fichier);
    return 0;
}
```

## Deep Dive (Plongée en profondeur)
Les CSV existent depuis les premières années de l'informatique, offrant un format d'échange de données qui résiste au temps grâce à sa simplicité. Alternatives: JSON, XML, mais ils apportent complexité et surcharge. Implémentation: le parsing (analyse) et la production de CSV doivent gérer les nuances telles que l'échappement de virgules et de guillemets.

## See Also (Voir aussi)
- Spécification RFC 4180 pour CSV : https://tools.ietf.org/html/rfc4180
- Tutoriel plus détaillé sur la manipulation des fichiers CSV en C : https://www.programmingsimplified.com/c/working-with-csv-files
- Comparaison de CSV avec d'autres formats (JSON, XML) : https://www.datahub.io/docs/data-packages/csv-vs-json-vs-xml
