---
title:    "Haskell: Extraction de sous-chaînes."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi 

Extraction de sous-chaînes en Haskell : pourquoi est-ce utile ? 

Extraction de sous-chaînes est une fonctionnalité importante en programmation, en particulier lorsqu'il s'agit de traiter des chaînes de caractères complexes. En Haskell, il existe plusieurs méthodes pour extraire des sous-chaînes, chacune offrant ses propres avantages et possibilités. Dans cet article, nous allons vous montrer pourquoi la manipulation de sous-chaînes est utile et comment le faire en Haskell de manière efficace. 

## Comment faire 

La fonction `drop` est souvent utilisée pour extraire une sous-chaîne à partir d'une chaîne de caractères en Haskell. Cette fonction prend en compte un nombre spécifique de caractères à supprimer à partir du début de la chaîne. Par exemple, si vous souhaitez extraire une sous-chaîne à partir du troisième caractère d'une chaîne, vous pouvez utiliser la fonction ainsi : 

```Haskell 
myString = "Bonjour le monde" 
extractedSubstring = drop 2 myString 
```

La sortie de ce code serait `njour le monde`, car le troisième caractère est `j` et nous avons utilisé `drop 2` pour sauter les deux premiers caractères `B` et `o`. 

Il existe également d'autres fonctions utiles pour extraire des sous-chaînes en Haskell, telles que `take` qui prend un nombre spécifique de caractères à partir du début de la chaîne, `splitAt` qui sépare une chaîne en deux parties à un index donné, et `substring` qui permet de spécifier un index de début et de fin pour extraire la sous-chaîne souhaitée. 

## Plongée en profondeur 

En Haskell, les sous-chaînes sont représentées par le type de données `String`, qui est en fait un synonyme de ` [Char]` (liste de caractères). Cela signifie que les fonctions de manipulation de listes peuvent également être utilisées pour manipuler des sous-chaînes. Par exemple, la fonction `reverse` peut être utilisée pour inverser une sous-chaîne ou la fonction `map` pour appliquer une fonction à chaque caractère de la sous-chaîne. 

Il est également important de noter que les sous-chaînes en Haskell sont des valeurs immuables, ce qui signifie qu'elles ne peuvent pas être modifiées après leur création. Cela peut sembler contre-intuitif pour certains programmeurs, mais cela correspond au fonctionnement général de la programmation fonctionnelle en Haskell. 

## Voir aussi 

Pour en savoir plus sur l'extraction de sous-chaînes en Haskell, vous pouvez consulter les ressources suivantes : 

- [Documentation officielle pour la fonction `drop`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:drop) 
- [Tutoriel vidéo sur la manipulation de listes en Haskell](https://www.youtube.com/watch?v=92S4zgXN17o) 

Maintenant que vous savez comment extraire des sous-chaînes en Haskell, vous pouvez les utiliser dans vos propres projets pour manipuler efficacement des chaînes de caractères. N'hésitez pas à explorer d'autres fonctions et méthodes pour trouver celle qui conviendra le mieux à votre cas d'utilisation. Bonne programmation !