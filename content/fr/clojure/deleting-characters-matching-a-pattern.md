---
title:                "Clojure: Suppression de caractères correspondants à un motif"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Parfois, dans nos programmes Clojure, nous avons besoin de supprimer des caractères spécifiques d'une chaîne de caractères ou d'une séquence. Cela peut être pour des raisons de nettoyage des données, de manipulation de texte ou pour tout autre besoin spécifique. Heureusement, Clojure offre des fonctionnalités puissantes pour effectuer cette tâche facilement.

## Comment faire

Supprimer des caractères correspondant à un modèle en Clojure peut être réalisé en utilisant la fonction `clojure.string/replace` avec une expression régulière. Par exemple, si nous voulons supprimer tous les chiffres d'une chaîne de caractères, nous pourrions utiliser l'expression régulière `#"[0-9]"` et la passer à la fonction `replace` comme ceci :

```Clojure
(clojure.string/replace "Bonjour123" #"[0-9]" "")
```

Cela renverra une chaîne de caractères contenant uniquement "Bonjour". Nous pouvons également utiliser cette méthode avec des séquences comme des listes ou des vecteurs. Par exemple, si nous avons une liste contenant des noms de personnes avec des chiffres, nous pouvons utiliser la même expression régulière pour supprimer les chiffres de tous les noms de la liste :

```Clojure
(defn remove-digits [names]
  (map #(clojure.string/replace % #"[0-9]" "") names))

(remove-digits ["Sarah123" "John456" "Lisa789"])
;; Output: ("Sarah" "John" "Lisa")
```

## Plongée en profondeur

Maintenant que nous avons vu comment supprimer des caractères correspondant à un modèle en Clojure, nous pouvons également utiliser des expressions régulières plus complexes avec la fonction `replace`. Par exemple, si nous voulons supprimer tous les caractères spéciaux d'une chaîne de caractères, nous pouvons utiliser l'expression régulière `#"[^a-zA-Z0-9]"` qui supprimera tous les caractères sauf les lettres et les chiffres. De plus, la fonction `replace` peut également prendre une lambda fonction en deuxième argument pour un remplacement plus avancé.

## Voir aussi

- Documentation officielle de la fonction `clojure.string/replace` : https://clojuredocs.org/clojure.string/replace
- Tutoriel sur les expressions régulières en Clojure : https://clojure.org/reference/regular_expressions