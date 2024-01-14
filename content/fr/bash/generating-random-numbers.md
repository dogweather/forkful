---
title:    "Bash: Génération de nombres aléatoires"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes peut-être en train d'apprendre la programmation Bash et vous vous demandez pourquoi vous devriez vous intéresser à générer des nombres aléatoires. Eh bien, il se trouve que générer des nombres aléatoires est un outil utile pour de nombreuses tâches, telles que les jeux, les simulations et les tests de performance.

## Comment faire

Pour générer des nombres aléatoires en Bash, nous allons utiliser la fonction `shuf`. Cette fonction prend en paramètre une chaîne de caractères contenant une liste de nombres séparés par des espaces et renvoie un nombre choisi au hasard dans cette liste. Voici un exemple de code:

```Bash
#!/bin/bash
# Ceci est un commentaire
echo "Le nombre aléatoire est:"
shuf -i 1-100 -n 1
```

Dans cet exemple, nous demandons à `shuf` de générer un nombre aléatoire compris entre 1 et 100 inclus. L'option `-n` spécifie le nombre de nombres à générer, dans notre cas nous voulons en générer un seul. Vous pouvez également utiliser cette fonction pour choisir au hasard un élément dans une liste, en utilisant l'option `-e` suivie de la liste d'éléments.

## Deep Dive

Alors comment `shuf` génère-t-il ces nombres aléatoires? En réalité, il utilise une source d'entropie, appelée générateur de nombres pseudo-aléatoires, pour choisir un nombre aléatoire dans la liste donnée. Cette source d'entropie est un algorithme qui utilise des calculs complexes et des données aléatoires pour générer des nombres apparemment aléatoires. Cependant, ces nombres ne sont pas vraiment aléatoires, ils sont simplement imprévisibles.

Vous pouvez également utiliser `shuf` avec une source d'entropie externe en utilisant l'option `-R`. Cela peut être utile si vous voulez que vos programmes génèrent des nombres aléatoires plus imprévisibles, par exemple lorsque vous effectuez des tests de sécurité.

# Voir Aussi

- [Documentation sur la fonction `shuf`](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Article sur la génération de nombres aléatoires en Bash] (https://www.cyberciti.biz/faq/unix-linux-random-number-generator-bash/)