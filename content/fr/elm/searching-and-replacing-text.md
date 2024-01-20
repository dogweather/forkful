---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---

## Quoi et Pourquoi?
La recherche et le remplacement de texte sont des outils essentiels pour manipuler les données de chaîne. Les programmeurs s'y mènent pour automatiser les modifications texte et améliorer l'efficacité du code.

## Comment ça marche:
La fonction String.replace vient à la rescousse dans Elm. Examinons ce code simple:
```Elm
import String 

main = 
    let 
        oldText = "J'aime le Elm"
        newText = String.replace "le" "la" oldText
    in 
        Html.text newText
```
Le code ci-dessus remplacera "le" par "la" dans la chaîne de caractères oldText. Ce qui donnera "J'aime la Elm".

## Pour aller plus loin:
La fonction de remplacement dans Elm est assez directe, mais il y a quelques détails à apprendre. Historiquement, le besoin de rechercher et de remplacer du texte est aussi ancien que la programmation elle-même. En Elm, la fonction String.replace utilise une approche directe et non regex pour le remplacement de texte - parfait pour les tâches simples mais peut-être limitatif pour des scénarios plus complexes. Si vous avez besoin d'une recherche plus puissante dans Elm, vous pouvez utiliser Regex à la place.

## A consulter également:
Pour plus d'information sur Elm, vous pouvez consulter les liens ci-dessous - 
   
[Documentation Elm](https://package.elm-lang.org/packages/elm/core/latest/String) 

[Guide pratique Elm](https://guide.elm-lang.org/)

[Tutoriel Elm Regex](https://package.elm-lang.org/packages/owanturist/elm-regex/latest/)