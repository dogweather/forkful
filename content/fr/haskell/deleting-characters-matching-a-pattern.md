---
title:                "Haskell: Suppression de caractères correspondant à un motif"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondant à un motif peut être utile pour nettoyer des données ou pour appliquer des transformations spécifiques sur une chaîne de caractères. Cela peut également être un exercice intéressant pour pratiquer vos compétences en programmation Haskell.

## Comment faire
La première étape pour supprimer des caractères correspondant à un motif est d'importer le module "Data.Text" dans votre code Haskell. Ensuite, vous pouvez utiliser la fonction "Data.Text.replace" pour remplacer tous les caractères correspondant à un motif donné par une chaîne vide. Regardons un exemple concret:

```Haskell
import Data.Text

-- Définir une chaîne de caractères initiale
let str = "Bonjour les programmeurs Haskell !"

-- Supprimer tous les espaces dans la chaîne
let newStr = Data.Text.replace " " "" str

-- Afficher la nouvelle chaîne de caractères
putStrLn newStr

-- Output: "BonjourlesprogrammeursHaskell!"
```

Vous remarquerez que toutes les occurrences de l'espace dans la chaîne initiale ont été supprimées. Vous pouvez également utiliser des expressions régulières pour définir des motifs plus complexes à supprimer.

## Plongée en profondeur
La fonction "Data.Text.replace" que nous avons utilisée dans l'exemple ci-dessus est en fait définie comme suit:

```Haskell
replace :: Text -> Text -> Text -> Text
```

Elle prend trois arguments: un motif à rechercher, une chaîne de caractères de remplacement et la chaîne de caractères initiale. Cette fonction utilise le type "Text" de Haskell, qui est optimisé pour la manipulation de données textuelles. Vous pouvez également utiliser la fonction "Data.Text.filter" pour supprimer des caractères en fonction d'un prédicat donné. Pour en savoir plus sur les fonctions de manipulation de données textuelles en Haskell, consultez la documentation officielle.

## Voir aussi
- [Documentation officielle de Data.Text](https://hackage.haskell.org/package/text-1.2.3.2/docs/Data-Text.html)
- [Tutoriel sur la manipulation de données textuelles en Haskell](https://wiki.haskell.org/Text_regex_examples)
- [Article sur les expressions régulières en Haskell](https://mmhaskell.com/blog/2017/9/6/efficient-text-regex-searches-in-haskell-with-pcre)