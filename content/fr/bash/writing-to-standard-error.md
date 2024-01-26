---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"Quoi et Pourquoi ?"

Écrire sur la sortie standard d'erreur (`stderr`) permet de séparer les messages d'erreur du flux principal (`stdout`). Les programmeurs le font pour faciliter le diagnostic et la gestion des erreurs.

## How to:
"Comment faire :"

Pour écrire sur `stderr` en Bash, redirigez votre message avec `>&2`. Voici un exemple :

```Bash
echo "Ceci est un message sur stdout"
echo "Ceci est un message d'erreur sur stderr" >&2
```

Si vous executez le script et redirigez avec `script.sh 2>error.log`, seule la deuxième ligne apparaîtra dans `error.log`.

## Deep Dive
"Plongée en profondeur"

À l'origine, Unix différencie `stdout` et `stderr` pour que les flux de données soient bien gérés. `stdout` est utilisé pour les sorties de données traditionnelles et `stderr` pour les messages d'erreur et de diagnostic.
Les alternatives pour la redirection incluent l'utilisation de la commande `tee` avec des descripteurs de fichiers ou la création de fonctions qui gèrent les logs. La redirection dans Bash utilise des descripteurs de fichiers, où `1` est le descripteur pour `stdout` et `2` pour `stderr`.

## See Also
"Voir également"

Pour en savoir plus, consultez les pages du manuel de Bash `man bash`, et pour de la lecture supplémentaire sur les redirections en Bash :

- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- The Linux Documentation Project (TLDP): https://tldp.org/
