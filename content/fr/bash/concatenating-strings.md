---
title:    "Bash: Concaténation de chaînes de caractères"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est une tâche courante en programmation Bash. Elle consiste à joindre plusieurs chaînes de caractères pour former une seule chaîne. Cette opération peut être utile pour créer des messages personnalisés, générer des noms de fichiers dynamiques, ou pour toute autre situation où vous avez besoin de combiner plusieurs textes.

## Comment faire

Pour concaténer des chaînes en Bash, vous pouvez utiliser l'opérateur "=". Voici un exemple de code:

```Bash
#!/bin/bash

prenom="Jean"
nom="Dupont"
echo "Bonjour, "$prenom" "$nom""
```

Dans cet exemple, nous déclarons deux variables, "prenom" et "nom", contenant respectivement les chaînes "Jean" et "Dupont". En utilisant l'opérateur "=", nous joignons ces deux variables avec un espace entre les deux, pour former la chaîne "Jean Dupont". Ensuite, nous utilisons la commande "echo" pour afficher cette chaîne à l'écran.

La sortie de ce code sera:

```Bash
Bonjour, Jean Dupont
```

Vous pouvez également concaténer des chaînes sans utiliser de variables, en utilisant directement la syntaxe suivante:

```Bash
#!/bin/bash

echo "Bonjour, ""Jean ""Dupont""
```

Dans cet exemple, nous avons simplement placé les différentes parties de la chaîne entre guillemets, avec un espace entre chaque partie, pour les joindre en une seule chaîne.

La sortie restera la même.

## Plongez plus profondément

En plus de l'opérateur "=", il existe également d'autres moyens de concaténer des chaînes en Bash. Vous pouvez utiliser la commande "printf", qui permet de formater une chaîne à partir de variables et de textes statiques:

```Bash
#!/bin/bash

prenom="Jean"
nom="Dupont"
printf "Bonjour, %s %s" $prenom $nom
```

La syntaxe "%s" sera remplacée par les valeurs des variables "prenom" et "nom". La sortie sera également la même que les exemples précédents.

## Voir aussi

- [Documentation Bash - Commandes de chaîne](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Formation Bash - Concaténer des chaînes](https://codefather.tech/blog/bash-concatenate-strings/)
- [Vidéo YouTube - Concaténation de chaînes en Bash](https://www.youtube.com/watch?v=ekQzHtTZCUA)