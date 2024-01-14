---
title:                "Haskell: Conversion d'une chaîne en minuscules"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il existe plusieurs raisons pour lesquelles on a besoin de convertir une chaîne de caractères en minuscules en programmation Haskell. La plus évidente est pour l'affichage de texte en minuscules afin d'améliorer la lisibilité pour les utilisateurs. Cela peut également être utile lors de la comparaison de chaînes de caractères, car cela assure une comparaison précise et uniforme.

## Comment faire

La conversion de chaînes de caractères en minuscules peut être facilement réalisée en utilisant la fonction "map" combinée avec la fonction intégrée "toLower". Voici un exemple de code pour cela :

```Haskell
import Data.Char (toLower) # Import de la fonction toLower pour pouvoir l'utiliser

toLowerString :: String -> String # Fonction pour convertir une chaîne de caractères en minuscules
toLowerString str = map toLower str # Utilisation de la fonction map avec toLower pour la transformation
```

Le résultat de cette fonction sera la chaîne de caractères en minuscules. Par exemple, si on applique cette fonction à "Haskell", le résultat sera "haskell".

## Plongée en profondeur

La fonction "map" est utilisée pour appliquer une fonction à chaque élément d'une liste. Dans notre cas, la fonction "toLower" est appliquée à chaque caractère de la chaîne. Cela signifie que la fonction traverse la chaîne de caractères et applique la fonction "toLower" à chaque caractère.

Il est important de noter que la conversion en minuscules dans Haskell est basée sur le standard Unicode, ce qui signifie que les caractères accentués et autres caractères spéciaux seront également convertis en minuscules.

Il est également possible de personnaliser la fonction "toLower" en utilisant des bibliothèques externes pour gérer les caractères spéciaux dans différentes langues. Cela peut être utile lors de la manipulation de données multilingues.

## Voir aussi

- [Documentation de la fonction map en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
- [Documentation de la fonction toLower en Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Guide de documentation pour la manipulation de chaînes de caractères en Haskell](https://wiki.haskell.org/Strings)