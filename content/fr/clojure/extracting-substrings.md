---
title:    "Clojure: Extraction de sous-chaînes"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Extraire des sous-chaînes (aussi appelées sous-chaînes) peut être utile lors de la manipulation de grandes chaînes de caractères dans vos programmes Clojure. Cela peut vous permettre de cibler et de manipuler des parties spécifiques d'une chaîne de caractères, ce qui peut simplifier le processus de traitement des données et rendre votre code plus efficace.

## Comment faire

Pour extraire des sous-chaînes en Clojure, nous pouvons utiliser la fonction `subs`. Cette fonction prend deux arguments : la chaîne de caractères à extraire des sous-chaînes et les indices de début et de fin des sous-chaînes souhaitées. Voici un exemple de code :

```Clojure
(def mon-chaine "Lorem ipsum dolor sit amet")

(subs mon-chaine 6 11)

```

La sortie de ce code sera `"ipsum"`, qui est la sous-chaîne située entre les caractères aux indices 6 et 11 (non inclus) de la chaîne originale.

Nous pouvons également utiliser des indices négatifs pour compter à partir de la fin de la chaîne. Par exemple, `subs mon-chaine -4` renverra `"amet"`, les 4 derniers caractères de la chaîne.

Il est également possible d'utiliser des caractères spéciaux pour extraire des sous-chaînes, tels que `:first` et `:last`. Par exemple, `subs mon-chaine :first 5` renverra les 5 premiers caractères de la chaîne, tandis que `subs mon-chaine :last 5` renverra les 5 derniers caractères.

## Plongée en profondeur

En plus de la fonction `subs`, Clojure dispose également d'autres fonctions utiles pour la manipulation de chaînes de caractères, telles que `replace` et `split`. Vous pouvez également utiliser des expressions régulières pour extraire des sous-chaînes en utilisant la fonction `re-find`.

Il est important de noter que l'extraction de sous-chaînes peut avoir un impact sur les performances de votre code, en particulier lors du traitement de grandes chaînes de caractères. Il est donc important de trouver le bon équilibre entre l'utilisation de fonctions de manipulation de chaînes et la performance globale de votre programme.

## Voir aussi

- [Documentation Clojure sur la fonction `subs`](https://clojuredocs.org/clojure.core/subs)
- [Exemples de manipulation de chaînes en Clojure](https://clojuredocs.org/quickref#string-functions)
- [Tutoriel sur les expressions régulières en Clojure](https://www.baeldung.com/clojure-regular-expressions)