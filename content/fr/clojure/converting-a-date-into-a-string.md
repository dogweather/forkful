---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "Clojure: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Quoi et Pourquoi?
La conversion d'une date en chaîne de caractères est un processus utilisé par les programmeurs pour convertir une date en un format lisible par l'ordinateur. Cela facilite la manipulation et la traitement des dates dans un programme, sans avoir à se soucier des différents formats et conventions de dates.

Comment faire:
Voici comment convertir une date en chaîne de caractères en utilisant Clojure:

```
(require '[clj-time.core :as time])

(def today (time/today))

(prn (str today))
(prn (time/format today "dd/MM/yyyy"))
```

Résultats de sortie:

```
#object[org.joda.time.LocalDate 0x719f2b14 "2021-09-26"]
"26/09/2021"
```

Plongée en Profondeur:
Historiquement, la manipulation des dates était un défi pour les programmeurs car différents pays et cultures utilisent différents formats de date. La conversion en chaîne de caractères a été introduite pour faciliter le traitement des dates dans les programmes.

Il existe plusieurs alternatives à la conversion de date en chaîne de caractères, telles que l'utilisation de bibliothèques externes dédiées à la manipulation des dates ou l'utilisation d'objets de date dans certains langages de programmation.

L'implémentation de la conversion de date en chaîne de caractères dans Clojure repose sur la bibliothèque Clj-Time, qui utilise la bibliothèque Joda-Time pour gérer les dates et les formats de date.

Voir aussi:
- La documentation officielle de Clojure sur la conversion de dates en chaînes de caractères: https://clojure.github.io/clj-time/#formatting-dates
- La documentation officielle de Joda-Time: https://www.joda.org/joda-time/
- Une comparaison des différentes façons de manipuler les dates en Clojure: https://gist.github.com/LouisFr/53a5d2e1255f03f6039ed315afbc1928