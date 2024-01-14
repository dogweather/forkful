---
title:    "Haskell: Concaténation de chaînes"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi
Il arrive souvent qu'en programmation, nous devons combiner plusieurs chaînes de caractères pour créer une seule chaîne. Cela peut être utile pour afficher des messages d'erreur avec des variables, créer des noms de fichiers dynamiquement, ou simplement pour construire des phrases complètes. Dans cet article, nous allons voir comment concaténer des chaînes de caractères en Haskell.

## Comment faire
Pour concaténer des chaînes de caractères en Haskell, nous utilisons l'opérateur "++". Voici un exemple de code:

```Haskell
string1 = "Bonjour"
string2 = "tout le monde"
concatenatedString = string1 ++ " " ++ string2
```

L'opérateur "++" combine les deux chaînes de caractères avec un espace entre elles pour créer une nouvelle chaîne "Bonjour tout le monde". Nous pouvons également concaténer plus de deux chaînes en utilisant plusieurs "++", comme ceci:

```Haskell
string1 = "Bonjour"
string2 = "à"
string3 = "toi"
concatenatedString = string1 ++ " " ++ string2 ++ " " ++ string3
```

Cette fois, nous obtenons la chaîne "Bonjour à toi".

Il est également possible de concaténer des chaînes avec des variables, comme ceci:

```Haskell
name = "Marie"
greeting = "Bonjour"
fullName = greeting ++ " " ++ name
```

Maintenant, notre chaîne finale sera "Bonjour Marie".

## Plongée en profondeur
En Haskell, les chaînes de caractères sont en fait des listes de caractères. Cela signifie que nous pouvons utiliser toutes les fonctions de liste pour manipuler des chaînes. Par exemple, nous pouvons utiliser la fonction "head" pour obtenir le premier caractère d'une chaîne, ou la fonction "tail" pour obtenir tous les caractères sauf le premier. Voici un exemple:

```Haskell
string = "Bonjour"
firstChar = head string
remainingString = tail string
```

Maintenant, "firstChar" sera égal à "B" et "remainingString" sera égal à "onjour".

Nous pouvons également utiliser la fonction "length" pour obtenir la longueur d'une chaîne de caractères, ou la fonction "reverse" pour inverser l'ordre des caractères. Par exemple:

```Haskell
string = "Bonjour"
stringLength = length string
reversedString = reverse string
```

Maintenant, "stringLength" sera égal à 7 et "reversedString" sera égal à "ruojnoB".

## Voir aussi
- [Documentation sur les strings en Haskell](https://www.haskell.org/tutorial/strings.html)
- [Guide de référence sur les opérateurs en Haskell](https://www.tutorialspoint.com/haskell/haskell_basic_operators.htm)
- [Tutoriel sur les listes en Haskell](https://www.tutorialspoint.com/haskell/haskell_lists.htm)