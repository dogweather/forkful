---
title:                "Clojure: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondant à un motif peut être utile lors de la manipulation de chaînes de caractères dans un programme Clojure. Cela peut nous permettre de supprimer des informations inutiles ou indésirables d'une chaîne de caractères afin de faciliter son utilisation.

## Comment faire
Voici un exemple de code en Clojure pour supprimer toutes les voyelles d'une chaîne de caractères :

```Clojure
(defn supprimer-voyelles [chaine]
  (clojure.string/replace chaine #"a|e|i|o|u" ""))
  
(supprimer-voyelles "Bonjour") ;; Output: "Bnjr"
```

Nous pouvons également utiliser des fonctions plus avancées pour supprimer des caractères selon des règles précises. Par exemple, la fonction `filter` peut être utilisée pour supprimer tous les caractères qui ne sont pas des lettres de la chaîne de caractères :

```Clojure
(defn supprimer-non-lettres [chaine]
  (clojure.string/replace (filter #(Character/isLetter %) chaine) #"[^a-zA-Z]" ""))
  
(supprimer-non-lettres "123abc$%&") ;; Output: "abc"
```

## Plongée en profondeur
La fonction `replace` utilisée dans les exemples ci-dessus peut prendre en paramètre un motif régulier pour supprimer des caractères selon un modèle précis. Nous pouvons utiliser différents motifs réguliers pour cibler différents types de caractères à supprimer. Par exemple, `#"[0-9]"` supprimerait tous les chiffres et `#"[^a-zA-Z]"` supprimerait tous les caractères qui ne sont pas des lettres. 

Il est important de noter que ces fonctions ne modifient pas la chaîne de caractères d'origine, mais renvoient une nouvelle chaîne modifiée. Il est donc recommandé de stocker le résultat dans une variable si nous voulons utiliser la chaîne modifiée dans notre programme.

## Voir aussi
- [Documentation officielle sur les chaînes de caractères en Clojure](https://clojure.org/reference/strings)
- [Site de référence pour les motifs réguliers en Clojure](https://regexone.com/references/clojure)