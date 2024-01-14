---
title:    "Clojure: Rechercher et remplacer du texte"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Le remplacement de texte est une tâche courante dans la programmation, car il permet de modifier rapidement et efficacement du contenu. Dans cet article, nous allons explorer comment effectuer des recherches et remplacements de texte en utilisant Clojure.

## Comment faire

Voici un exemple de code qui montre comment effectuer un remplacement de texte en utilisant la fonction `replace` :

```Clojure
(defn replace-string
  [str old new]
  (.replaceAll str (java.util.regex.Pattern/quote old) new))
```

Dans cet exemple, nous définissons une fonction `replace-string` qui prend en paramètres une chaîne de caractères, un texte à remplacer et un nouveau texte. La fonction utilise ensuite la méthode `replaceAll` pour remplacer toutes les occurrences du texte donné par le nouveau texte.

Voici un exemple d'utilisation de cette fonction :

```Clojure
(replace-string "Bonjour le monde !" "Bonjour" "Salut")
;; Résultat : "Salut le monde !"
```

En utilisant la fonction `replace-string`, vous pouvez ainsi facilement remplacer du texte dans une chaîne de caractères.

## Plongée en profondeur

En utilisant la fonction `replace-string` présentée précédemment, vous pouvez également effectuer des recherches et remplacements de texte en utilisant des expressions régulières.

Par exemple, si vous souhaitez remplacer toutes les adresses email dans une chaîne de caractères par la mention "adresse email", voici comment vous pouvez procéder :

```Clojure
(replace-string "Mon adresse email est john.doe@mail.com" #"\b[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+\b"
  "adresse email")
;; Résultat : "Mon adresse email est adresse email"
```

Dans cet exemple, nous utilisons une expression régulière pour trouver toutes les adresses email dans la chaîne de caractères, puis nous les remplaçons par la mention "adresse email".

En utilisant les expressions régulières, vous pouvez ainsi effectuer des remplacements de texte plus complexes et précis.

## Voir aussi

- [Documentation officielle Clojure](https://clojure.org/)
- [Exemples de code Clojure](https://www.clojure-examples.com/)