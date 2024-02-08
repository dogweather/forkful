---
title:                "Travailler avec TOML"
date:                  2024-02-03T18:12:16.079216-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

TOML (Tom's Obvious, Minimal Language ou Langage Minimal et Évident de Tom) est un format de fichier de configuration qui est facile à lire grâce à sa sémantique claire. Les programmeurs l'utilisent pour les fichiers de configuration dans les applications parce que sa simplicité et sa lisibilité pour les humains en font un excellent choix par rapport à des formats comme XML ou JSON dans certains contextes.

## Comment faire :

Pour travailler avec TOML en C, vous avez d'abord besoin d'une bibliothèque capable d'analyser les fichiers TOML, puisque la bibliothèque standard C n'inclut pas cette fonctionnalité. Un choix populaire est `tomlc99`, un analyseur TOML léger pour C99. Voici un guide rapide pour lire un fichier de configuration TOML simple :

Tout d'abord, assurez-vous d'avoir installé `tomlc99` et de l'avoir correctement lié à votre projet.

**Fichier TOML d'exemple (`config.toml`) :**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Code C pour analyser ce fichier :**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Impossible d'ouvrir le fichier");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Erreur lors de l'analyse du fichier\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Serveur de base de données : %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Port %d : %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Sortie :**
```
Serveur de base de données : "192.168.1.1"
Port 0 : 8001
Port 1 : 8001
Port 2 : 8002
```

## Plongée en profondeur

TOML a été créé par Tom Preston-Werner, co-fondateur de GitHub, comme une réponse aux limites qu'il percevait dans d'autres formats de fichier de configuration. Son objectif est d'être simple et sans ambiguïté, tant pour les humains que pour les ordinateurs, à lire et à écrire sans avoir besoin de règles d'analyse complexes. Dans l'écosystème C, TOML n'est pas un citoyen de première classe comme il pourrait l'être dans des langages de plus haut niveau tels que Rust avec son `serde_toml` ou Python avec `toml`, qui ont des bibliothèques avec un support natif. Plutôt, les développeurs C doivent s'appuyer sur des bibliothèques externes comme `tomlc99`, mais cela est typique étant donné l'accent mis par C sur le minimalisme et la performance.

Bien que TOML soit loué pour sa clarté, lors du choix d'un format de fichier de configuration, il est essentiel de considérer les besoins du projet. Dans des scénarios nécessitant des structures plus complexes ou de l'interactivité avec des API web, JSON ou même YAML pourraient offrir un meilleur ajustement malgré leur complexité accrue. TOML brille dans des configurations où la lisibilité et la simplicité sont primordiales, pas nécessairement là où les structures de données les plus avancées sont nécessaires.
