---
title:                "Capitaliser une chaîne de caractères."
html_title:           "Clojure: Capitaliser une chaîne de caractères."
simple_title:         "Capitaliser une chaîne de caractères."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est & pourquoi ?
 
 Capitaliser une chaîne de caractères signifie simplement mettre la première lettre de chaque mot en majuscule. Les programmeurs le font souvent pour améliorer la lisibilité des noms de variables ou de fonctions dans leur code.

Comment faire :

```Clojure
(let [str "mon code en clojure"]    
  (.toUpperCase str)) 
```
Résultat : "Mon Code En Clojure"

```Clojure
(map #(.toUpperCase %) ["un" "deux" "trois"]) 
```
Résultat : ("Un" "Deux" "Trois")


Deep Dive :

La pratique de la capitalisation des chaînes de caractères remonte à l'époque des machines à écrire, où les majuscules étaient plus visibles que les minuscules. Dans le monde de la programmation, il existe également des alternatives telles que le camel case (PremiereLettreMinusculeDeChaqueMot) ou le snake case (premiere_lettre_minuscule_de_chaque_mot). La méthode de capitalisation que vous choisissez dépendra du style adopté par votre équipe de développement.

Voir aussi :

Voir la documentation officielle de Clojure pour plus d'informations sur la fonction .toUpperCase : https://clojuredocs.org/clojure.core/toUpperCase

Vous pouvez également consulter cet article pour en savoir plus sur les différentes conventions en matière de nommage : https://nils-blum-oeste.net/coding-style-and-naming-practices-for-clojure/