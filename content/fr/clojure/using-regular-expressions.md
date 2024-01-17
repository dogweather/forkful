---
title:                "Utiliser les expressions régulières"
html_title:           "Clojure: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?
Les expressions régulières sont des séquences de caractères qui permettent de rechercher et de manipuler du texte en utilisant des motifs spécifiques. Les programmeurs utilisent les expressions régulières pour simplifier et accélérer la manipulation de texte dans leurs programmes.

Comment faire?
Clojure offre de nombreuses façons de travailler avec les expressions régulières. Voici quelques exemples courants :
```Clojure
(re-seq #"([A-Z])\w+" "Hi there, my name is John")
; => ("Hi" "John")

(re-find #"(\d{3})-(\d{3})-(\d{4})" "123-456-7890")
; => ["123-456-7890" "123" "456" "7890"]

(re-matches #"Hello (\w+)" "Hello world")
; => ["Hello world" "world"]
```

Plongez plus en profondeur :
Les expressions régulières ont été inventées dans les années 1950 par le mathématicien américain Stephen Kleene. Bien qu'elles soient largement utilisées dans de nombreux langages de programmation, elles peuvent être difficiles à lire et à comprendre pour les débutants. Heureusement, il existe également des alternatives pour manipuler du texte, telles que les fonctions de manipulation de chaînes de caractères intégrées à Clojure. Les expressions régulières utilisent également des concepts avancés de traitement de texte tels que les groupes de capture, qui permettent de récupérer des parties spécifiques d'un texte correspondant.

Voir aussi :
- Guide de référence de l'API Clojure regex : https://clojure.github.io/api/#clojure.string/replace
- Tutoriel de base sur les expressions régulières : https://www.regular-expressions.info/tutorial.html