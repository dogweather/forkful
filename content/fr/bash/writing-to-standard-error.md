---
title:                "Bash: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles on pourrait vouloir écrire vers la sortie d'erreur standard (stderr) en Bash. Cela peut être utile lors du débogage d'un script ou lors de la gestion des erreurs dans un programme. Quelle que soit la raison, il peut être utile de connaître les bases de cette fonctionnalité pour un programmeur Bash.

## Comment Faire

Pour écrire vers stderr en Bash, il suffit simplement d'utiliser la commande `>&2` suivie du contenu que vous souhaitez écrire. Voici un exemple:

```Bash
echo "Ceci est un exemple d'erreur" >&2
```

Cela écrira "Ceci est un exemple d'erreur" vers stderr plutôt que vers la sortie standard (stdout). Vous pouvez également rediriger la sortie standard vers stderr en utilisant le symbole `1>&2`. Voici un autre exemple:

```Bash
ls fichier_inexistant.txt 1>&2
```

Cela écrira un message d'erreur lorsqu'il essayera de lister un fichier inexistant.

## Plongée Profonde

Il est important de noter que stderr est une sortie différente de stdout et qu'ils peuvent être gérés séparément. Par exemple, si vous utilisez le symbole `|` pour rediriger la sortie d'un programme vers un autre, stderr ne sera pas redirigé. Il est également possible de rediriger stderr vers un fichier en utilisant le symbole `2>`.

Il est également possible de combiner stderr et stdout en utilisant `2>&1` pour rediriger les deux vers la même sortie. Cela peut être utile dans certaines situations, mais il est important de comprendre que les messages d'erreur seront alors mélangés avec la sortie standard.

## Voir Aussi

Pour en savoir plus sur l'écriture vers stderr en Bash, consultez ces liens utiles:

- Tutoriel Bash de la Documentation officielle: https://linuxconfig.org/bash-scripting-tutorial
- Documentation Bash sur les redirections: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Utiliser une sortie d'erreur pour le débogage: https://www.cyberciti.biz/tips/debugging-shell-script.html