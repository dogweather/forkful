---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:52.716977-07:00
description: "Les tableaux associatifs, ou maps, vous permettent de stocker des donn\xE9\
  es sous forme de paires cl\xE9-valeur, facilitant ainsi l'organisation et la\u2026"
lastmod: '2024-03-13T22:44:58.314477-06:00'
model: gpt-4-0125-preview
summary: "Les tableaux associatifs, ou maps, vous permettent de stocker des donn\xE9\
  es sous forme de paires cl\xE9-valeur, facilitant ainsi l'organisation et la r\xE9\
  cup\xE9ration des informations par cl\xE9."
title: Utilisation des tableaux associatifs
weight: 15
---

## Quoi & Pourquoi ?

Les tableaux associatifs, ou maps, vous permettent de stocker des données sous forme de paires clé-valeur, facilitant ainsi l'organisation et la récupération des informations par clé. Ils sont pratiques lorsque vous avez besoin d'une manière plus structurée de gérer des données que de simples listes, notamment dans les configurations et lorsque vous traitez une gamme d'attributs.

## Comment faire :

Fish ne supporte pas nativement les tableaux associatifs comme Bash 4+, mais vous pouvez obtenir une fonctionnalité similaire en utilisant une combinaison de listes et de manipulation de chaînes. Voici comment les imiter :

D'abord, configurer séparément les éléments du "tableau associatif" :

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Pour accéder à un élément, référez-vous-y directement :

```Fish Shell
echo $food_color_apple
# Sortie : red
```

Si vous avez besoin de les parcourir, utilisez une boucle for en tenant compte d'une convention de nommage :

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Sortie :
# red
# yellow
```

Pour ceux qui regrettent la commande de Bash `${!array[@]}` pour obtenir toutes les clés, vous pouvez stocker les clés dans une liste séparée :

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'est' $food_color_$key
end
# Sortie :
# apple est red
# banana est yellow
```

## Approfondissement

De véritables tableaux associatifs comme dans d'autres langages de script ne font pas encore partie de l'approche de Fish. La solution de contournement présentée tire parti des capacités de manipulation de chaînes et de listes de Fish pour créer une structure pseudo-associative. Bien que fonctionnelle, elle n'est pas aussi propre ou à l'épreuve des erreurs que le serait le support intégré des tableaux associatifs. D'autres shells comme Bash et Zsh fournissent une fonctionnalité de tableau associatif intégrée, ce qui résulte en un code plus direct et lisible. Cependant, la philosophie de conception de Fish vise la simplicité et la convivialité, possiblement au détriment de telles fonctionnalités. La solution de contournement satisfait la plupart des besoins mais gardez un œil sur l'évolution de Fish Shell - ses développeurs améliorent activement et ajoutent des fonctionnalités basées sur les retours de la communauté.
