---
title:    "Haskell: Trouver la longueur d'une chaîne de caractères"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi

Le calcul de la longueur d'une chaîne peut sembler simple à première vue, mais il est en fait un exercice commun et utile en programmation fonctionnelle. Il peut vous aider à mieux comprendre certaines fonctions de base et à résoudre des problèmes réels.

# Comment faire

Dans Haskell, il existe plusieurs façons de trouver la longueur d'une chaîne. La méthode la plus simple consiste à utiliser la fonction `length` qui prend en entrée une liste et renvoie son nombre d'éléments. Dans ce cas, nous pouvons passer notre chaîne en tant que liste de caractères.

```Haskell
length "Bonjour" -- Sortie : 7
```

Si vous souhaitez écrire votre propre fonction pour calculer la longueur d'une chaîne, vous pouvez utiliser une fonction récursive et incrémenter un compteur à chaque appel jusqu'à ce que la chaîne soit vide.

```Haskell
tailLength :: [a] -> Int
tailLength [] = 0
tailLength (_:xs) = 1 + tailLength xs

tailLength "Bonjour" -- Sortie : 7
```

# Plongée en profondeur

Il est important de noter que la fonction `length` ne fonctionne pas uniquement sur les listes de caractères, mais sur tous les types de données avec une instance de la classe de type `Foldable`. Cela signifie que vous pouvez l'utiliser avec d'autres types de données tels que des tableaux, des ensembles ou même des arbres.

De plus, il est intéressant de mentionner que la fonction `length` a une complexité temporelle linéaire, ce qui signifie que la durée d'exécution est proportionnelle à la taille de la liste. Cela peut être problématique pour les listes très longues, mais dans la plupart des cas, cela ne pose pas de problème.

# Voir aussi

- [Documentation officielle sur la fonction `length`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Tutoriel sur les fonctions récursives en Haskell](https://www.geeksforgeeks.org/recursion-in-haskell/)
- [Article sur la complexité temporelle en Haskell](https://wiki.haskell.org/Time_complexity_in_Haskell)