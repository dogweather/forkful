---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:53.679898-07:00
description: "\xC9crire sur l'erreur standard en C implique de diriger les messages\
  \ d'erreur et les informations de diagnostic vers un flux distinct de la sortie\u2026"
lastmod: '2024-03-11T00:14:32.262058-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire sur l'erreur standard en C implique de diriger les messages d'erreur\
  \ et les informations de diagnostic vers un flux distinct de la sortie\u2026"
title: "\xC9criture sur l'erreur standard"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard en C implique de diriger les messages d'erreur et les informations de diagnostic vers un flux distinct de la sortie principale du programme. Les programmeurs font cela pour séparer les messages d'erreur de la sortie standard, rendant les deux plus faciles à lire et à traiter séparément, surtout lors du débogage ou de la journalisation de l'exécution des programmes.

## Comment faire :

En C, le flux `stderr` est utilisé pour écrire des messages d'erreur. Contrairement à l'écriture sur la sortie standard avec `printf`, l'écriture sur `stderr` peut se faire en utilisant `fprintf` ou `fputs`. Voici comment vous pouvez le faire :

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Ceci est un message d'erreur.\n");

    fputs("Ceci est un autre message d'erreur.\n", stderr);
    
    return 0;
}
```

Exemple de sortie (sur stderr) :
```
Ceci est un message d'erreur.
Ceci est un autre message d'erreur.
```

Il est important de noter que bien que la sortie semble similaire à `stdout` dans la console, lorsque la redirection est utilisée dans le terminal, la distinction devient claire :

```sh
$ ./votre_programme > sortie.txt
```

Cette commande redirige uniquement la sortie standard vers `sortie.txt`, tandis que les messages d'erreur apparaîtront toujours à l'écran.

## Approfondissement

La distinction entre `stdout` et `stderr` dans les systèmes basés sur Unix remonte aux premiers jours de C et Unix. Cette séparation permet une gestion des erreurs et une journalisation plus robustes, car elle permet aux programmeurs de rediriger les messages d'erreur indépendamment de la sortie standard du programme. Alors que `stderr` n'est pas tamponné par défaut pour garantir une sortie immédiate des messages d'erreur, ce qui aide au débogage des plantages et d'autres problèmes critiques, `stdout` est généralement tamponné, ce qui signifie que sa sortie pourrait être retardée jusqu'à ce que le tampon soit vidé (par exemple, à l'achèvement du programme ou à la vidange manuelle).

Dans les applications modernes, écrire sur `stderr` reste pertinent, en particulier pour les outils en ligne de commande et les applications serveur où la distinction entre les messages de journalisation réguliers et les erreurs est cruciale. Cependant, pour une gestion des erreurs plus complexe, en particulier dans les applications GUI ou là où des mécanismes de journalisation plus sophistiqués sont nécessaires, les programmeurs pourraient utiliser des bibliothèques de journalisation dédiées qui offrent plus de contrôle sur le formatage des messages, les destinations (par exemple, fichiers, réseau) et les niveaux de gravité (info, avertissement, erreur, etc.).

Bien que `stderr` fournisse un mécanisme fondamental pour le signalement d'erreurs en C, l'évolution des pratiques de programmation et la disponibilité de cadres de journalisation avancés signifient qu'il est souvent juste le point de départ pour les stratégies modernes de gestion des erreurs.
