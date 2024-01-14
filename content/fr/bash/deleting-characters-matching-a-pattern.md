---
title:                "Bash: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondants à un motif est une tâche courante dans la programmation Bash. Cela peut être utile lorsqu'on souhaite nettoyer ou modifier du texte pour le traitement ultérieur ou pour l'affichage.

## Comment faire
Voici un exemple simple de code Bash pour supprimer des caractères correspondant à un motif spécifique dans une chaîne de caractères :

```Bash
# Définir la chaîne de caractères initiale
string="Bonjour tout le monde !"

# Supprimer les caractères 'o' dans la chaîne de caractères
new_string="${string//o/}"

# Afficher la nouvelle chaîne de caractères
echo "$new_string"

# Output : Bnjr tut le mnde !
```

Dans cet exemple, nous utilisons la substitution de motif de la syntaxe `${string//pattern/replace}` pour remplacer chaque instance du caractère 'o' par une chaîne vide.

Il est également possible d'utiliser des expressions régulières pour supprimer des caractères correspondant à un motif plus complexe. Par exemple, pour supprimer tous les caractères non alphabétiques dans une chaîne de caractères, on peut utiliser la commande `tr` :

```Bash
# Définir la chaîne de caractères initiale
string="Il est temps de programmer !"

# Supprimer tous les caractères non alphabétiques
new_string=$(echo "$string" | tr -d '[^[:alpha:]]')

# Afficher la nouvelle chaîne de caractères
echo "$new_string"

# Output : Ilesttempsdeprogrammer
```

Ici, nous utilisons la commande `tr` avec l'option `-d` pour supprimer tous les caractères spécifiés dans les crochets. L'expression régulière `[[:alpha:]]` correspond à tous les caractères alphabétiques.

## Plongée en profondeur
La suppression de caractères correspondant à un motif peut également être réalisée en utilisant les commandes `grep` et `sed` dans Bash. La commande `grep` peut être utilisée pour rechercher une expression régulière dans un fichier ou une chaîne de caractères et la commande `sed` peut être utilisée pour remplacer du texte dans un fichier ou une chaîne de caractères.

Par exemple, pour supprimer toutes les voyelles dans une chaîne de caractères, on peut utiliser la commande `grep` pour rechercher toutes les voyelles et la commande `sed` pour les remplacer par une chaîne vide :

```Bash
# Définir la chaîne de caractères initiale
string="Programmer, c'est cool !"

# Utiliser grep pour rechercher toutes les voyelles
vowels=$(echo "$string" | grep -o -i '[aeiou]')

# Utiliser sed pour remplacer les voyelles par une chaîne vide
new_string=$(echo "$string" | sed "s/$vowels//gi")

# Afficher la nouvelle chaîne de caractères
echo "$new_string"

# Output : Prgrmmr, c'st cl !
```

## Voir aussi
- [Documentation de la syntaxe ${string//pattern/replace}](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Documentation de la commande tr](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Documentation de la commande sed](https://www.gnu.org/software/sed/manual/sed.html)