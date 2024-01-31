---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Manipuler des CSV (valeurs séparées par des virgules), c'est gérer des données tabulaires comme un tableau Excel, directement dans nos scripts. Les programmeurs font ça pour automatiser et scripter le traitement de données massives et structurées.

## Comment faire :
Voici comment lire, écrire et parser des CSV en Fish :

Lire un fichier CSV :
```Fish Shell
awk -F ',' '{print $1, $2}' fichier.csv
```

Écrire dans un fichier CSV :
```Fish Shell
echo 'colonne1,colonne2' > nouveau_fichier.csv
echo 'donnée1,donnée2' >> nouveau_fichier.csv
```

Parser avec `cut` :
```Fish Shell
cut -d ',' -f 1 fichier.csv
```

Résultat d'un `cut` :
```
Nom
Alice
Bob
```

## Analyse détaillée
Les CSV existent depuis les premiers jours de l'informatique personnelle, une manière simple de stocker des infos structurées. Des alternatives comme JSON ou XML proposent plus de structure, mais requièrent des parsers plus complexes. En Fish, on exploite des outils Unix comme `awk`, `cut`, et `sed` pour gérer les CSV, car Fish n'a pas de built-in dédié pour cela.

## Voir Aussi
- La page de manuel `man csvkit` pour un outil dédié CSV en ligne de commande.
- Documentation Fish [sur son site web officiel](https://fishshell.com/docs/current/index.html) pour des scripts plus avancés.
- Le guide pragmatique de `awk`: [AWK Guides](https://awk.js.org) pour les manipulations complexes de texte.
