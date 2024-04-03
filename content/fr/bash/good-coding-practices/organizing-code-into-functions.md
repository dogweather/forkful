---
date: 2024-01-26 01:09:02.789254-07:00
description: "Comment faire : Cr\xE9ez une fonction simple en Bash ."
lastmod: '2024-03-13T22:44:58.002644-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9ez une fonction simple en Bash."
title: Organisation du code en fonctions
weight: 18
---

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
