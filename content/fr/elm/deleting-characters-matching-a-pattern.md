---
title:                "Suppression des caractères correspondant à un modèle"
html_title:           "Elm: Suppression des caractères correspondant à un modèle"
simple_title:         "Suppression des caractères correspondant à un modèle"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes peut-être tombé sur cette tâche ennuyeuse : supprimer toutes les occurrences d'un certain caractère ou motif dans une chaîne de caractères. Heureusement, il existe une solution simple en Elm qui vous permettra de vous en débarrasser rapidement et efficacement.

# Comment faire

```Elm
deletePattern : String -> Char -> String
deletePattern str c =
   String.filter (\char -> char /= c) str

deletePattern "Hello World" 'l'
-- Output: "Heo Word"
```

Voici un exemple de code qui supprime toutes les lettres "l" de la chaîne de caractères "Hello World". Vous pouvez remplacer le motif et la chaîne de caractères par ceux de votre choix. La fonction ```deletePattern``` utilise la fonction prédéfinie ```String.filter``` pour parcourir la chaîne de caractères et supprimer toutes les occurrences du caractère spécifié.

# Plongée en profondeur

Si vous souhaitez supprimer plusieurs caractères ou motifs à la fois, vous pouvez utiliser une variante de la fonction ```deletePattern```. Voici un exemple de code qui utilise une liste de caractères à supprimer :

```Elm
deletePatterns : String -> List Char -> String
deletePatterns str cs =
   String.filter (\char -> not <| List.member char cs) str

deletePatterns "Hello World" ['l', 'o']
-- Output: "He Wrd"
```

La fonction ```List.member``` vérifie si un caractère est présent dans la liste. Si ce n'est pas le cas, la fonction ```not``` retourne ```True``` et le caractère est conservé dans la chaîne de caractères résultante.

# Voir aussi

Pour en savoir plus sur les fonctions de manipulation de chaînes de caractères en Elm, consultez la documentation officielle : [String](https://package.elm-lang.org/packages/elm/core/latest/String) et [List](https://package.elm-lang.org/packages/elm/core/latest/List).