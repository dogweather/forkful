---
title:    "Bash: Extraction de sous-chaînes"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Pourquoi extraire des sous-chaînes dans Bash?

L'extraction de sous-chaînes est une technique utile en programmation Bash qui permet de récupérer une partie spécifique d'une chaîne de caractères. Cela peut être utile pour manipuler des données, extraire des informations précises ou pour réaliser des tâches automatisées. Dans cet article, nous allons explorer pourquoi et comment extraire des sous-chaînes en utilisant Bash.

## Comment faire

L'extraction de sous-chaînes peut être réalisée en utilisant la commande `cut` ou en utilisant des expressions régulières avec la commande `sed`. Voici un exemple de code pour extraire une sous-chaîne avec `cut`:

```Bash
# Déclaration de la chaîne de caractères
str="Bonjour tout le monde!"
# Extraction de la sous-chaîne à partir de la 8ème position jusqu'à la fin
echo $str | cut -c8-
# Sortie: tout le monde!
```

Et voici un exemple utilisant `sed` pour extraire une sous-chaîne en utilisant une expression régulière:

```Bash
# Déclaration de la chaîne de caractères
str="abcdefg12345"
# Extraction de la sous-chaîne contenant uniquement les chiffres
echo $str | sed -E 's/[a-z]+//g'
# Sortie: 12345
```

Ces exemples sont simples, mais en combinant différentes options et en utilisant des variables, il est possible de créer des scripts complexes pour extraire des sous-chaînes à partir de données plus compliquées.

## Plongée en profondeur

L'extraction de sous-chaînes peut être réalisée en utilisant des indices numériques pour spécifier les positions de début et de fin de la sous-chaîne. Cela peut également être fait en utilisant des expressions régulières pour délimiter la sous-chaîne à extraire. De plus, des options supplémentaires peuvent être spécifiées pour modifier le comportement de la commande, par exemple en spécifiant un séparateur différent que le caractère par défaut pour `cut`.

Il est également important de noter que les chaînes de caractères peuvent être de différentes longueurs, il est donc souvent nécessaire de manipuler les indices ou expressions régulières en conséquence pour obtenir des sous-chaînes précises.

# Voir aussi

- [Documentation officielle de la commande `cut`](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [Documentation officielle de la commande `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Guide débutant sur les expressions régulières en Bash](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)