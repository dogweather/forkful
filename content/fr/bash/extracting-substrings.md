---
title:    "Bash: Extraction de sous-chaînes"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extrayez des sous-chaînes de caractères est une tâche courante en programmation Bash. Cela peut être utile pour sélectionner une partie spécifique d'une chaîne plus longue pour une utilisation ultérieure ou pour comparer les similitudes entre deux chaînes.

## Comment faire

Pour extraire une sous-chaîne en Bash, vous pouvez utiliser la commande `cut` en spécifiant la position de début et de fin de la sous-chaîne ainsi que la chaîne d'entrée. Par exemple, si nous voulons extraire les trois premiers caractères d'une chaîne, nous pouvons utiliser la commande suivante :

```Bash
cut -c 1-3 << "Ma chaîne de caractères"
```

Cela nous donnera en sortie "Ma ".

Pour extraire une sous-chaîne en utilisant une expression régulière, nous pouvons utiliser la commande `grep` avec l'option `-o` pour ne retourner que la partie de la chaîne qui correspond à l'expression régulière. Par exemple, pour extraire tous les nombres d'une chaîne, nous pouvons utiliser la commande suivante :

```Bash
grep -o "[0-9]+" << "Ma chaîne de chiffres 12345"
```

Cela nous donnera en sortie "12345".

## Plongée en profondeur

Il est également possible d'extraire des sous-chaînes en utilisant des variables. Par exemple, si nous avons une variable nommée `nom` contenant "Jean Dupont", nous pouvons extraire le prénom et le nom en utilisant la commande `cut` et en spécifiant un délimiteur, qui dans ce cas serait un espace :

```Bash
prenom="$(cut -d " " -f 1 << "$nom")"
nom_de_famille="$(cut -d " " -f 2 << "$nom")"
```

Cela assignera "Jean" à la variable `prenom` et "Dupont" à la variable `nom_de_famille`.

Vous pouvez également utiliser des opérations de substitution de commandes pour extraire une sous-chaîne spécifique d'une variable. Par exemple, pour extraire les trois derniers caractères d'un mot contenant 6 caractères, nous pouvons utiliser la commande suivante :

```Bash
variable="abcdef"
sous_chaine="${variable: -3}"
echo "$sous_chaine" // Résultat : "def"
```

## Voir aussi

- Documentation de la commande `cut` : https://www.tutorialspoint.com/unix_commands/cut.htm
- Documentation de la commande `grep` : https://www.tutorialspoint.com/unix_commands/grep.htm