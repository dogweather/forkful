---
title:                "Fish Shell: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

# Pourquoi

CSV (Comma Separated Values) est un format de fichier couramment utilisé pour stocker des données tabulaires telles que des feuilles de calcul ou des bases de données. Si vous travaillez avec des données tabulaires sur un système Linux, vous pourriez vous demander comment vous pouvez facilement manipuler et traiter ces fichiers en utilisant la ligne de commande. Dans cet article, nous allons explorer comment utiliser Fish Shell pour travailler avec des fichiers CSV.

## Comment faire

Il existe plusieurs façons d'interagir avec des fichiers CSV en utilisant Fish Shell. Voici quelques exemples de code pour vous montrer comment vous pouvez accomplir différentes tâches.

### Lire un fichier CSV

Pour lire un fichier CSV dans Fish Shell, nous allons utiliser la commande `read` qui accepte deux arguments : le nom du fichier et le séparateur des colonnes (par défaut, c'est une virgule). Voici un exemple de code pour lire un fichier CSV contenant les informations de fruits :

```
cat fruits.csv
pommes,bananes,oranges
cerises,fraises,kiwis
```

```
read -c "" fruits.csv | while read -l line
  echo $line
end
```

La sortie devrait ressembler à ceci :

```
pommes
bananes
oranges
cerises
fraises
kiwis
```

### Manipuler les données du CSV

La commande `csv` est très utile pour manipuler les données d'un fichier CSV. Elle peut être utilisée pour trier, filtrer, choisir des colonnes spécifiques et même fusionner plusieurs fichiers CSV. Voici un exemple de code pour trier un fichier CSV contenant des noms et des âges par ordre alphabétique :

```
csv sort -c 1 file.csv
```

La sortie devrait ressembler à ceci :

```
Alice,25
Bob,30
Charlie,18
```

### Créer un nouveau fichier CSV

Vous pouvez également créer un nouveau fichier CSV en utilisant Fish Shell. Voici un exemple de code pour créer un nouveau fichier `pays.csv` contenant des noms de pays et leurs capitales :

```
set -l countries "France;Paris" "Espagne;Madrid" "Italie;Rome"
for country in $countries
  set -l name (string split -m 1 ";" $country)
  set -l capital (string split -m 2 ";" $country)
  echo "$name,$capital" >> pays.csv
end
```

## Plongée profonde

Si vous voulez approfondir vos connaissances sur la manipulation de fichiers CSV en utilisant Fish Shell, vous pouvez explorer la documentation officielle de Fish ou consulter des ressources en ligne telles que des tutoriels ou des forums de discussion.

# Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/)
- [Tutoriel sur l'utilisation de Fish Shell avec CSV](https://easyautodidacte.com/fish-shell-et-csv/)
- [Forum de discussion Fish Shell](https://github.com/fish-shell/fish-shell/issues)