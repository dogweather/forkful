---
title:    "Haskell: Extraction de sous-chaînes"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un programmeur Haskell, vous savez probablement déjà que le langage offre de puissants outils pour gérer les chaînes de caractères. Cependant, il peut parfois être utile d'extraire une partie spécifique d'une chaîne pour des raisons telles que le traitement de données ou la manipulation de texte. Dans cet article, nous allons plonger dans la façon d'extraire des sous-chaînes en Haskell et voir pourquoi cela peut être utile.

## Comment faire
Pour extraire une sous-chaîne en Haskell, nous utilisons la fonction ```take``` et ```drop```. Voici un exemple de code qui utilise ces fonctions pour extraire une sous-chaîne de la chaîne "Bonjour Monde" :

```Haskell
let str = "Bonjour Monde"
let subs = take 7 (drop 2 str)
print subs
```

La sortie de ce code serait ```jour Mo```, car nous avons ignoré les deux premiers caractères et pris les sept caractères suivants. Nous pouvons également utiliser la fonction ```dropEnd``` pour ignorer les caractères à la fin de la chaîne. Par exemple :

```Haskell
let str = "Sous-chaîne"
let subs = take 9 (dropEnd 4 str)
print subs
```

La sortie serait "Sous-ch", car nous avons ignoré les quatre derniers caractères et pris les neuf premiers. Nous pouvons également utiliser la fonction ```takeWhile``` pour extraire une sous-chaîne jusqu'à ce qu'une certaine condition soit remplie. Par exemple :

```Haskell
let str = "98765abcd"
let subs = takeWhile isDigit str
print subs
```

La sortie serait "98765", car nous avons pris tous les caractères tant qu'ils sont des chiffres. En outre, nous pouvons également utiliser des conditions plus complexes avec la fonction ```dropWhile```.

## Plongée en profondeur
En plus des fonctions mentionnées ci-dessus, Haskell offre également des outils tels que ```splitAt``` pour séparer une chaîne en deux parties à un certain index, et ```break``` pour séparer une chaîne jusqu'à ce qu'un certain caractère soit rencontré. De plus, pour ceux qui ont besoin de gérer des chaînes multilingues, il existe des fonctions spéciales telles que ```takeText``` et ```takeWhile1``` qui prennent en compte les différences de caractères entre les langues.

## Voir aussi
- [Documentation Haskell sur les chaînes de caractères](https://www.haskell.org/tutorial/string.html)
- [Tutoriel vidéo sur l'utilisation des chaînes de caractères en Haskell](https://www.youtube.com/watch?v=mzM7170XJEE)
- [Discussion sur la manipulation de chaînes en Haskell](https://stackoverflow.com/questions/20995945/substring-in-haskell)