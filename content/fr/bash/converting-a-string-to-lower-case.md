---
title:                "Bash: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules est une tâche courante en programmation et peut être utile dans de nombreuses situations. Cela peut être utilisé pour rendre une entrée utilisateur insensible à la casse, pour comparer des chaînes de caractères avec une plus grande précision ou simplement pour des raisons d'esthétique dans la sortie d'un programme. Dans cet article, nous allons découvrir comment réaliser cette tâche en Bash.

## Comment Faire

Pour convertir une chaîne en minuscules en Bash, nous pouvons utiliser la commande `tr` avec l'option `-s` pour ignorer les doublons et l'option `-s` pour indiquer que la transformation doit être effectuée sur chaque caractère. Voyons un exemple de code pour mieux comprendre:

```Bash
#!/bin/bash

# Déclarer une variable avec une chaîne en majuscules
my_string="BONJOUR TOUT LE MONDE"

# Utiliser tr pour convertir la chaîne en minuscules
my_new_string=$(echo $my_string | tr -s '[:upper:]' '[:lower:]')

# Afficher la nouvelle chaîne en minuscules
echo $my_new_string
```

Dans cet exemple, nous avons déclaré une variable avec une chaîne en majuscules, puis nous avons utilisé la commande `tr` pour convertir cette chaîne en minuscules. Nous avons stocké le résultat dans une nouvelle variable et l'avons affiché à l'aide de la commande `echo`. Le résultat sera:

```
bonjour tout le monde
```

Il est également possible d'utiliser une boucle pour parcourir chaque caractère de la chaîne et le convertir en minuscule à l'aide de la commande `printf`:

```Bash
#!/bin/bash

# Déclarer une variable avec une chaîne en majuscules
my_string="BONJOUR TOUT LE MONDE"

# Déclarer une variable pour stocker la nouvelle chaîne
my_new_string=""

# Utiliser une boucle pour parcourir chaque caractère
for (( i=0; i<${#my_string}; i++ )); do
    # Convertir le caractère en minuscule et l'ajouter à la nouvelle chaîne
    my_new_string+=$(printf "%c" "${my_string:$i:1}" | tr '[:upper:]' '[:lower:]')
done

# Afficher la nouvelle chaîne en minuscules
echo $my_new_string
```

Le résultat sera le même que dans le premier exemple.

## Plongée Profonde

En utilisant la commande `tr`, nous pouvons également effectuer d'autres transformations sur la chaîne, comme remplacer les caractères spéciaux par des lettres ou retirer certains caractères. Par exemple, pour remplacer tous les espaces par des traits d'union, nous pouvons utiliser la commande suivante:

```Bash
tr -s ' ' '-'
```

Nous pouvons également utiliser la commande `sed` pour remplacer des caractères spécifiques dans une chaîne. Par exemple, pour remplacer tous les caractères spéciaux par des espaces, nous pouvons utiliser:

```Bash
sed 's/[!@#$%^&*()]//g'
```

## Voir Aussi

Pour en savoir plus sur les commandes `tr` et `sed` en Bash, vous pouvez consulter les liens suivants (en anglais):

- [Bash tr command](https://linux.die.net/man/1/tr)
- [Bash sed command](https://linux.die.net/man/1/sed)