---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Les expressions régulières filtrent et manipulent du texte selon un schéma défini. Les développeurs les utilisent pour la recherche, la validation, et le traitement de données textuelles de manière rapide et concise.

## Comment faire :
```Elm
import Regex

-- Trouver des mots qui commencent par "Elm"
let
    regex = Maybe.withDefault Regex.never <| Regex.fromString "^Elm\\w*"
in
    Regex.find regex "Elm est super. ElmLang rocks!"
-- Résultat : [Match {match = "Elm", index = 0, submatches = []}, Match {match = "ElmLang", index = 14, submatches = []}]

-- Vérifier si un format de courriel est valide
let
    regex = Maybe.withDefault Regex.never <| Regex.fromString "^[\\w._%+-]+@[\\w.-]+\\.[a-zA-Z]{2,}$"
in
    Regex.contains regex "exemple@domaine.com"
-- Résultat : True
```

## Plongée en Profondeur
Historiquement, les expressions régulières datent des années 1950, mais leur popularité a augmenté avec les langages de script dans les années 1980. En Elm, `Regex` est un module core qui offre une interface typée aux expressions régulières. Des alternatives incluent l'usage de fonctions de chaîne de caractères ou des bibliothèques tierces pour des cas simples. Elm gère les expressions régulières dans une manière sûre et prévisible, évitant les pièges commun à d'autres langages comme le "backtracking" excessif.

## Voir Aussi
- Documentation Elm de `Regex`: https://package.elm-lang.org/packages/elm/core/latest/Regex
- MDN Web Docs sur les expressions régulières: https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions
- Elm Lang Slack pour poser des questions: http://elmlang.herokuapp.com/
