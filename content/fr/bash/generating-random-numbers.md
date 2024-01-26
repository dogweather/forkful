---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:27.195511-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Générer des nombres aléatoires, c'est créer des chiffres imprévisibles. Les programmeurs s'en servent pour tout, des jeux aux simulations, en passant par la sécurité informatique.

## How to: (Comment faire :)
```Bash
# Générer un nombre aléatoire entre 1 et 100
echo $((RANDOM%100+1))

# Générer un nombre aléatoire avec openssl pour une meilleure entropie
echo $(openssl rand -hex 3 | od -An -N3 -i)
```
Exemple de sortie :
```
42
789456
```

## Deep Dive (Plongée en profondeur)
Avant `$RANDOM`, il fallait bricoler avec des dates ou des fichiers pour de l'aléatoire. `$RANDOM` est simple mais imparfait, notamment pour la cryptographie. Si vous avez besoin de plus sûr, utilisez `openssl` ou `/dev/urandom`. L'implémentation du générateur dépend du système et sa qualité varie.

## See Also (Voir aussi)
- Man page de Bash pour `$RANDOM`: https://man7.org/linux/man-pages/man1/bash.1.html#Shell_Variables
- Article de Red Hat sur la génération de nombres aléatoires : https://www.redhat.com/sysadmin/random-numbers-bash
- OpenSSL Documentation : https://www.openssl.org/docs/manmaster/man1/openssl-rand.html
