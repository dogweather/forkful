---
title:                "Extraction de sous-chaînes"
html_title:           "Clojure: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Clojure, il peut arriver que vous ayez besoin d'extraire des sous-chaînes de votre chaîne principale. Que ce soit pour les manipuler, les comparer ou les utiliser dans d'autres fonctions, comprendre comment extraire des sous-chaînes peut rendre votre code plus efficace et votre travail plus facile.

## Comment faire

Pour extraire une sous-chaîne, vous pouvez utiliser la fonction "subs" qui prend trois arguments : la chaîne d'origine, l'indice de départ et l'indice de fin de la sous-chaîne souhaitée. Voici un exemple de code avec des commentaires pour expliquer chaque étape :

```Clojure
(def chaine "Bonjour tout le monde")

; Utilisation de la fonction "subs" pour extraire une sous-chaîne qui démarre à l'indice 8
; et s'arrête avant l'indice 17 (exclu)
(subs chaine 8 17)

; Résultat : "le monde"

; Vous pouvez aussi utiliser des nombres négatifs pour compter à partir de la fin de la chaîne.
; Pour extraire la dernière partie de la chaîne :
(subs chaine -6)
; Résultat : "monde"
```

Bien sûr, vous pouvez également utiliser des variables pour stocker les indices de début et de fin, ce qui peut être utile si vous avez besoin d'extraire des sous-chaînes dans une boucle ou dans le cadre d'une fonction plus complexe.

## Plongée en profondeur

La fonction "subs" peut sembler simple, mais elle a en fait quelques subtilités à connaître. Tout d'abord, les indices utilisés pour délimiter la sous-chaîne sont inclusifs pour l'indice de départ et exclusifs pour celui de fin. Cela signifie que la sous-chaîne sera composée des caractères compris entre l'indice de départ (inclus) et l'indice de fin (exclu).

De plus, si vous utilisez des nombres négatifs pour les indices, ils sont comptés à partir de la fin de la chaîne plutôt qu'à partir du début. Cela peut être utile lorsque vous ne connaissez pas la longueur exacte de la chaîne, mais que vous savez que vous voulez extraire les derniers caractères.

Enfin, gardez à l'esprit que la fonction "subs" ne modifie pas la chaîne d'origine, elle renvoie simplement la sous-chaîne extraite. Si vous avez besoin de modifier la chaîne d'origine, vous pouvez utiliser la fonction "replace" avec "subs".

## Voir aussi

Pour plus d'informations sur la fonction "subs" et les chaînes de caractères en général, voici quelques liens utiles :

- [Documentation officielle de la fonction "subs"](https://clojuredocs.org/clojure.core/subs)
- [Article sur l'utilisation des chaînes en Clojure](https://clojure.org/reference/strings)