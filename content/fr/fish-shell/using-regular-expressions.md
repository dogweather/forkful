---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Les expressions régulières sont des séquences de caractères formant un modèle de recherche, utilisées pour manipuler du texte : chercher, remplacer, extraire des informations. Les programmeurs s'en servent pour gagner en efficacité et en précision lorsqu'ils travaillent avec du texte.

## Comment faire :
Nous allons matcher des patterns dans du texte. Prenons quelques exemples :

```Fish Shell
# Rechercher si un mot existe dans une chaîne
echo "Le poisson nage dans l'eau" | string match -r "poisson"
# Output: poisson

# Trouver tous les chiffres dans une chaîne
echo "Il y a 42 façons d'utiliser fish" | string match -r "[0-9]+"
# Output: 42

# Remplacer un mot par un autre
echo "Fish est meilleur que Bash" | string replace -r "meilleur" "plus rapide"
# Output: Fish est plus rapide que Bash
```

## Plongée en profondeur :
Les expressions régulières existent depuis les années 1950, liées au formalisme mathématique. En programmation, on les utilise depuis les outils Unix des années 70. En Fish, `string match` et `string replace` sont intégrés et proposent une syntaxe simplifiée pour manipuler des chaînes de caractères, sans devoir recourir à `sed` ou `grep` comme dans d'autres shells. Fish utilise d'ailleurs une regex engine compatible avec Perl-Compatible Regular Expressions (PCRE), qui est assez puissante.

## Voir également :
- Documentation officielle de Fish sur la manipulation de chaînes de caractères : [https://fishshell.com/docs/current/cmds/string.html]
- Tutoriel sur les expressions régulières : [https://www.regular-expressions.info/tutorial.html]
- Outil en ligne pour tester les expressions régulières : [https://regex101.com]
