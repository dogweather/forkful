---
title:                "Travailler avec TOML"
date:                  2024-01-26T04:19:31.157826-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML est un langage de sérialisation de données conçu pour être facile à lire et à écrire. Les programmeurs l'utilisent pour les fichiers de configuration, le stockage simple de données, et l'échange de données entre langages grâce à sa clarté et son aspect convivial pour l'humain.

## Comment faire :
Analysons un fichier de configuration TOML en C en utilisant la bibliothèque "tomlc99". D'abord, installez la bibliothèque. Ensuite, créez un `config.toml` :

```toml
titre = "Exemple TOML"

[propriétaire]
nom = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Maintenant, analysons-le en C :

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Erreur : impossible d'ouvrir le fichier de configuration\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Erreur : %s\n", errbuf);
        return 1;
    }

    printf("Titre : %s\n", toml_raw_in(conf, "titre"));

    toml_table_t* propriétaire = toml_table_in(conf, "propriétaire");
    printf("Nom du Propriétaire : %s\n", toml_raw_in(propriétaire, "nom"));

    toml_free(conf);
    return 0;
}
```
Exemple de sortie :
```
Titre : "Exemple TOML"
Nom du Propriétaire : "Tom Preston-Werner"
```

## Exploration Approfondie
TOML, qui signifie Tom's Obvious, Minimal Language (Langage Minimal et Évident de Tom), a été créé par Tom Preston-Werner en 2013. Il sert d'alternative plus simple aux formats tels que XML et YAML, en se concentrant sur être plus lisible et inscriptible par les humains. Bien que JSON soit une autre alternative, TOML conserve une structure plus facile à analyser visuellement par les humains, ce qui est l'une des principales raisons de son adoption dans les fichiers de configuration.

En C, travailler avec TOML implique de choisir une bibliothèque d'analyse syntaxique puisque le langage ne le prend pas en charge nativement. Des bibliothèques comme "tomlc99" sont conformes à C99 et fournissent une API pour décoder le texte TOML. Lorsqu'on considère la performance, une gestion adéquate des erreurs et de la mémoire est cruciale car C ne dispose pas de collecte de déchets intégrée.

## Voir Aussi :
1. Spécification TOML : [https://toml.io/fr/](https://toml.io/fr/)
2. Dépôt GitHub tomlc99 : [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Comparaison des formats de sérialisation de données : [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)