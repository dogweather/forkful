---
title:                "Conversion d'une chaîne de caractères en minuscules"
aliases:
- fr/clojure/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:05.449760-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertir une chaîne en minuscules, c'est transformer tous les caractères alphabétiques en leur équivalent minuscule. Pourquoi ? Pour uniformiser les données avant comparaison ou traitement, éliminant ainsi les problèmes de casse.

## How to:
Clojure rend la conversion super facile avec la fonction `clojure.string/lower-case`. Jetons un œil :

```Clojure
(require '[clojure.string :as str])

; Convertir une chaîne simple
(str/lower-case "Bonjour MONDE")
; => "bonjour monde"

; L'appliquer à une collection de chaînes
(map str/lower-case ["ABC" "Déjà Vu" "123"])
; => ("abc" "déjà vu" "123")
```

## Deep Dive
Avant `clojure.string/lower-case`, des approches plus manuelles étaient nécessaires. Historiquement, la manipulation de texte est un problème classique, où chaque langue l'a réinventé à sa manière. L'avantage de Clojure est sa simplicité, et l'utilisation de Java interne pour la gestion des chaînes offre la performance.

Les alternatives incluent l'utilisation de regex pour remplacer manuellement ou utiliser des fonctions personnalisées pour parcourir et convertir les chaînes. Pour la coulisse, `clojure.string/lower-case` se base sur la méthode Java `toLowerCase()`, qui prend en compte les règles de localité (Locale) pour la conversion des caractères.

## See Also
Pour plus d'informations, consultez :

- La documentation officielle de Clojure sur les chaînes : [clojure.string API](https://clojuredocs.org/clojure.string)
- Informations sur les locales et la manipulation de texte en Java: [Oracle Java Locale](https://docs.oracle.com/javase/tutorial/i18n/locale/)
