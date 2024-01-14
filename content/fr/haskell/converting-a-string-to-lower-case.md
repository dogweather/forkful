---
title:    "Haskell: Convertir une chaîne en minuscules"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation en Haskell, il est parfois nécessaire de convertir une chaîne de caractères en minuscules pour effectuer des opérations de manipulation de texte. Cela peut être utile pour comparer des chaînes de caractères ou pour afficher du texte de manière uniforme.

# Comment Faire

Il existe une fonction intégrée en Haskell appelée "toLower" qui permet de convertir une chaîne en minuscules. Voici un exemple de code pour montrer comment l'utiliser :

```Haskell
-- Définition d'une chaîne de caractères
let string = "BONJOUR"

-- Utilisation de la fonction toLower pour convertir en minuscules
let lowerString = toLower string

-- Affichage du résultat
print lowerString 

-- Output : "bonjour"
```

Comme vous pouvez le voir dans l'exemple, en utilisant la fonction "toLower", la chaîne de caractères "BONJOUR" a été convertie en "bonjour". Il est également possible de convertir en minuscules une chaîne de caractères entrée par l'utilisateur en utilisant la fonction "getLine".

# Approfondissement

Il est important de noter que la fonction "toLower" ne peut être utilisée que sur des chaînes de caractères en ASCII. Si vous avez besoin de convertir une chaîne de caractères en minuscules qui contient des caractères spéciaux ou des caractères non-ASCII, il est recommandé d'utiliser la bibliothèque "Data.Text". Cette bibliothèque offre des fonctions de manipulation de texte plus avancées et prend en charge une variété de types de caractères.

En outre, il peut être utile d'utiliser la fonction "map" en conjonction avec la fonction "toLower" pour convertir chaque caractère individuellement en minuscules dans une chaîne de caractères.

# Voir Aussi

- La documentation officielle pour la fonction toLower : https://hackage.haskell.org/package/base/docs/Data-Char.html#v:toLower
- La bibliothèque Data.Text : https://hackage.haskell.org/package/text
- Documentation sur la fonction map : https://hackage.haskell.org/package/base/docs/Prelude.html#v:map