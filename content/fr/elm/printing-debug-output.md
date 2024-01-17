---
title:                "Affichage des sorties de débogage"
html_title:           "Elm: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

L'impression de sortie de débogage est une pratique courante pour les programmeurs Elm. Elle consiste à afficher des informations sur l'état et le fonctionnement du programme dans la console du navigateur. Cela permet aux développeurs de comprendre plus facilement ce qui se passe dans leur code et de détecter les éventuels problèmes.

# Comment faire:

Voici un exemple simple d'utilisation de l'impression de sortie de débogage en Elm:

```
elm-sandbox 0.19.1

import Html exposing (text)
import Debug

main =
  text (Debug.toString 42)
```

Le résultat de cette impression sera ```42``` dans la console du navigateur.

# Plongée en profondeur:

L'impression de sortie de débogage était déjà présente dans la version précédente d'Elm, mais elle a été améliorée dans la version actuelle. Il existe également d'autres alternatives pour le débogage en Elm, telles que l'utilisation d'outils de débogage externes ou la mise en place de tests unitaires. Pour mettre en place l'impression de sortie de débogage, Elm utilise la fonction ```toString``` pour convertir les valeurs en chaînes de caractères.

# Voir aussi:

Pour plus d'informations sur l'impression de sortie de débogage en Elm, consultez la documentation officielle de Elm sur le débogage: https://guide.elm-lang.org/debugging/