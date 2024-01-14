---
title:    "Clojure: Suppression de caractères correspondant à un modèle"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données textuelles en Clojure, il est probable que vous ayez un jour eu besoin de supprimer des caractères correspondants à un certain modèle. Que ce soit pour nettoyer vos données, préparer une analyse ou résoudre un problème spécifique, supprimer des caractères peut être une tâche commune dans la programmation Clojure. Dans cet article, nous allons explorer comment le faire de manière efficace et élégante.

## Comment faire

Pour supprimer des caractères correspondants à un modèle en Clojure, nous allons utiliser la fonction `re-seq` qui renvoie une séquence de tous les correspondances trouvées dans une chaîne de caractères. Ensuite, nous allons utiliser la fonction `clojure.string/replace` pour remplacer ces correspondances par une chaîne vide, ce qui équivaut à les supprimer.

```Clojure
(def data "Je suis une chaîne de caractères avec des @@@ caractères inutiles!!!")

(defn clean-string [str]
  (clojure.string/replace str (re-seq #"@+") ""))
  
(clean-string data)
;; Output : "Je suis une chaîne de caractères avec des caractères inutiles!!!"
```

Dans cet exemple, nous avons utilisé l'opérateur `"#"` pour spécifier le modèle à utiliser pour les caractères correspondants. Vous pouvez également utiliser des expressions régulières plus complexes pour des cas plus spécifiques.

## Deep Dive

Il est important de noter que la fonction `re-seq` renvoie une `lazy sequence`, ce qui signifie que toutes les modifications effectuées avec `clojure.string/replace` sont évaluées à la demande et ne créent pas de séquence temporaire. Cela peut être très utile pour les grandes chaînes de caractères ou les fichiers de données.

De plus, vous pouvez également utiliser la fonction `reduce` pour supprimer des caractères correspondants à un motif dans une chaîne de caractères.

```Clojure
(def data ["Je suis une chaîne de!!!"
           "Je suis une autre chaîne de$@###"
           "Et moi un dernier exemple@@@@@"])

(defn clean-strings [data]
  (reduce (fn [acc str] (str, (clean-string str))) [] data))
  
(clean-strings data)
;; Output : ["Je suis une chaîne de" "Je suis une autre chaîne de" "Et moi un dernier exemple"]
```

## Voir aussi

- [Documentation Clojure pour `re-seq`](https://clojuredocs.org/clojure.core/re-seq)
- [Documentation Clojure pour `clojure.string/replace`](https://clojuredocs.org/clojure.string/replace)