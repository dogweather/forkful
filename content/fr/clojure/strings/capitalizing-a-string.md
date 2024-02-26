---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:58.896473-07:00
description: "La capitalisation d'une cha\xEEne implique de modifier la cha\xEEne\
  \ afin que son premier caract\xE8re soit en majuscule, tandis que le reste de la\
  \ cha\xEEne demeure\u2026"
lastmod: '2024-02-25T18:49:54.153035-07:00'
model: gpt-4-0125-preview
summary: "La capitalisation d'une cha\xEEne implique de modifier la cha\xEEne afin\
  \ que son premier caract\xE8re soit en majuscule, tandis que le reste de la cha\xEE\
  ne demeure\u2026"
title: "Mettre en majuscule une cha\xEEne"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La capitalisation d'une chaîne implique de modifier la chaîne afin que son premier caractère soit en majuscule, tandis que le reste de la chaîne demeure inchangé. Les programmeurs réalisent souvent la capitalisation des chaînes pour assurer la cohérence des données, spécialement pour les noms et les lieux, ou pour se conformer aux règles grammaticales dans les interfaces utilisateurs.

## Comment faire :
Clojure, étant un langage JVM, vous permet d'utiliser directement les méthodes Java String. Voici un exemple de base de comment capitaliser une chaîne en Clojure :

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure n'inclut pas de fonction intégrée spécifiquement pour la capitalisation des chaînes, mais comme montré, vous pouvez facilement y parvenir en combinant les fonctions `clojure.string/upper-case`, `subs`, et `str`.

Pour une solution plus concise et pour gérer des manipulations de chaînes plus complexes, vous pourriez vous tourner vers une bibliothèque tierce. Une telle bibliothèque populaire dans l'écosystème Clojure est `clojure.string`. Cependant, lors de ma dernière mise à jour, elle n'offre pas de fonction `capitalize` directe au-delà de ce qui est démontré avec les fonctionnalités de base de Clojure, donc la méthode montrée ci-dessus est votre approche directe sans faire appel à des bibliothèques supplémentaires spécifiquement pour la capitalisation.

Souvenez-vous, lorsque vous travaillez avec des chaînes en Clojure qui interagissent avec les méthodes Java, vous travaillez effectivement avec des chaînes Java, ce qui vous permet de tirer parti de tout l'arsenal des méthodes Java String directement dans votre code Clojure si nécessaire.
