---
title:                "Interpoler une chaîne de caractères"
html_title:           "Haskell: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le fait-on?

Interpoler une chaîne est une technique utilisée par les programmeurs pour insérer des valeurs dynamiquement dans un message ou une phrase prédéfinie. Cela permet de personnaliser les messages en fonction des variables ou des données fournies lors de l'exécution du programme.

# Comment faire:

Voici un exemple de code Haskell montrant comment interpoler une chaîne :

```
let name = "John"
putStrLn $ "Bonjour " ++ name ++ "!"
```
Ce code affichera "Bonjour John!" lorsque vous l'exécuterez. La séquence ``` ++ ``` est utilisée pour concaténer ou fusionner les chaînes ensemble. Dans cet exemple, la variable ``` name ``` est insérée dans la phrase pour créer un message personnalisé.

# Plongée en profondeur:

Historiquement, l'interpolation de chaîne a été introduite dans le langage de programmation Perl et a ensuite été adoptée par d'autres langages tels que Ruby, Python et Haskell. Cette méthode est souvent préférée à la concaténation car elle améliore la lisibilité du code et permet une meilleure manipulation des données.

Une alternative populaire à l'interpolation de chaîne est l'utilisation de modèles ou de gabarits, qui sont des structures de données spéciales contenant des emplacements pour les valeurs à insérer. Cependant, dans certains cas, l'interpolation de chaîne peut être plus simple et plus efficace.

# Voir aussi:

Pour en savoir plus sur l'interpolation de chaîne en Haskell, consultez la documentation officielle sur les chaînes de caractères : https://www.haskell.org/documentation/#strings

Vous pouvez également trouver des exemples pratiques et des tutoriels sur les sites tels que le wiki Haskell ou StackOverflow.