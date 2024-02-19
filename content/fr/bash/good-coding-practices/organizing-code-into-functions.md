---
aliases:
- /fr/bash/organizing-code-into-functions/
date: 2024-01-26 01:09:02.789254-07:00
description: "Diviser le code en fonctions signifie d\xE9composer les scripts en blocs\
  \ plus petits, r\xE9utilisables, qui r\xE9alisent des t\xE2ches sp\xE9cifiques.\
  \ Cela rend le code\u2026"
lastmod: 2024-02-18 23:09:09.025375
model: gpt-4-1106-preview
summary: "Diviser le code en fonctions signifie d\xE9composer les scripts en blocs\
  \ plus petits, r\xE9utilisables, qui r\xE9alisent des t\xE2ches sp\xE9cifiques.\
  \ Cela rend le code\u2026"
title: Organisation du code en fonctions
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Diviser le code en fonctions signifie décomposer les scripts en blocs plus petits, réutilisables, qui réalisent des tâches spécifiques. Cela rend le code plus propre, plus compréhensible et plus facile à déboguer.

## Comment faire :
Créez une fonction simple en Bash :

```Bash
greet() {
  echo "Bonjour, $1 !"
}
```

Utilisez-la en appelant la fonction avec un paramètre :

```Bash
greet "Monde"  # Sortie : Bonjour, Monde !
```

Les fonctions peuvent retourner des valeurs en utilisant `return` pour des codes de statut numériques (et non pour un retour de données réelles) :

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # Sortie : 7
```

Notez que `$?` capture la valeur de retour de la dernière commande, qui est le résultat numérique de `add`.

## Approfondissement
En Bash, les fonctions sont un moyen de compartimenter le code depuis les premières versions. Historiquement, l'utilisation des fonctions s'aligne sur les principes de programmation structurée introduits dans les années 1960 pour améliorer la qualité du code.

Les alternatives aux fonctions incluent l'inclusion de fichiers de script ou l'utilisation d'alias, mais elles n'offrent pas le même niveau de modularité et de réutilisation.

Un détail de mise en œuvre notable en Bash est que les fonctions sont des citoyens de première classe ; elles n'ont pas de mot-clé de déclaration spécifique comme `function` dans d'autres langues, bien que `function` soit facultatif en Bash pour la lisibilité. La portée des fonctions est également intéressante : les variables sont globales par défaut, à moins qu'elles ne soient déclarées comme locales, ce qui peut entraîner un comportement inattendu si elles ne sont pas gérées correctement.

## Voir également
- Manuel Bash sur les fonctions Shell : https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Guide de script avancé en Bash : https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" pour des concepts et des pratiques de script de fonctions approfondies.
