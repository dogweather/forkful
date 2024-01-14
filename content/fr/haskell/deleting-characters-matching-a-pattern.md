---
title:    "Haskell: Suppression de caractères correspondant à un motif"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile lorsque l'on souhaite nettoyer ou filtrer une chaîne de caractères. Cela peut également être nécessaire pour convertir une chaîne en un format spécifique.

## Comment Faire

Pour supprimer des caractères correspondant à un modèle en Haskell, on peut utiliser la fonction `filter`. Cette fonction prend en paramètre une fonction prédicat pour déterminer si un caractère doit être supprimé ou non, ainsi qu'une liste de caractères à traiter.

Voici un exemple de code pour supprimer tous les nombres d'une chaîne de caractères :

```Haskell
-- Définition de la fonction prédicat
estNombre :: Char -> Bool
estNombre c = c >= '0' && c <= '9'

-- Utilisation de la fonction "filter" pour supprimer les nombres de la chaîne
let result = filter (not . estNombre) "abc123" -- "abc"

-- Affichage du résultat
print result -- "abc"
```

Dans cet exemple, la fonction prédicat `estNombre` vérifie si un caractère est un nombre en vérifiant s'il se situe entre les caractères "0" et "9". La fonction `filter` renvoie ensuite une liste ne contenant que les caractères ne correspondant pas au prédicat.

Il est également possible d'utiliser une fonction anonyme pour définir la fonction prédicat :

```Haskell
let result = filter (\c -> c /= '@' && c /= '$') "abc@123" -- "abc123"
```

Dans cet exemple, la fonction anonyme vérifie si un caractère est différent de "@" ou de "$" avant de renvoyer une liste ne contenant que les caractères ne correspondant pas à ces deux caractères spécifiques.

## Plongée en Profondeur

En plus de la fonction `filter`, Haskell dispose également de la fonction `deleteBy` qui permet de supprimer les éléments d'une liste en utilisant une fonction d'équivalence. Cela peut être utile lorsque l'on souhaite supprimer des éléments en fonction d'un critère plus complexe qu'une simple égalité.

Voici un exemple de code utilisant la fonction `deleteBy` :

```Haskell
-- Définition de la fonction d'équivalence
estMajuscule :: Char -> Char -> Bool
estMajuscule c d = toUpper c == d

-- Utilisation de la fonction "deleteBy" pour supprimer toutes les lettres majuscules de la chaîne
let result = deleteBy estMajuscule 'A' "Hello World" -- "ello World"

-- Affichage du résultat
print result -- "ello World"
```

Dans cet exemple, la fonction d'équivalence `estMajuscule` compare deux caractères en les convertissant en majuscules avant de les comparer. La fonction `deleteBy` supprime ensuite toutes les occurrences du caractère spécifié en utilisant cette fonction d'équivalence.

## Voir Aussi

- Documentation officielle de Haskell : https://www.haskell.org/documentation/
- Tutoriel sur les fonctions de manipulation de listes en Haskell : https://wiki.haskell.org/List_function_examples