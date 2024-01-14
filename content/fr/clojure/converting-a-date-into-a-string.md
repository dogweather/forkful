---
title:                "Clojure: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pouvez avoir besoin de convertir une date en chaîne de caractères dans votre code Clojure. Cela peut être utile pour l'affichage à l'utilisateur, la manipulation et l'enregistrement de données, ou pour générer des rapports en utilisant des formats spécifiques de date.

## Comment procéder

Il existe plusieurs méthodes pour convertir une date en chaîne de caractères en Clojure, en voici quelques exemples :

```Clojure
; Utilisation de la fonction format-date du package clojure.java-time
(require '[java-time :as t])
(t/format-date (t/local-date "2021-03-10") "dd/MM/yyyy") 
; Résultat : "10/03/2021"

; Utilisation de la fonction str du package clojure.string
(require '[clojure.string :as str])
(str/join "-" ["10" "03" "2021"]) 
; Résultat : "10-03-2021"

; Utilisation de la fonction format du package clj-time
(require '[clj-time.format :as f])
(f/unparse (f/formatter "dd/MM/yyyy") (f/local-date "2021-03-10")) 
; Résultat : "10/03/2021"
```

## Approfondissement

La méthode la plus simple pour convertir une date en chaîne de caractères en Clojure est d'utiliser la fonction str du package clojure.string. Cependant, cette méthode peut être limitée si vous avez besoin de formats de date spécifiques ou d'une manipulation plus complexe des données. Dans ce cas, il est recommandé d'utiliser les packages clojure.java-time ou clj-time qui offrent une plus grande flexibilité et des fonctions spécifiques pour les opérations sur les dates.

N'oubliez pas que la conversion peut être différente selon le format de date que vous utilisez. Par exemple, si vous avez besoin de la date au format "dd/MMM/yyyy" (ex. "10/Mar/2021"), vous devriez d'abord convertir votre date en une représentation locale et utiliser ensuite la fonction format pour obtenir le résultat souhaité.

## Voir aussi

- [Documentation officielle de clojure.java-time](https://github.com/dm3/clojure.java-time)
- [Documentation officielle de clj-time](https://github.com/clj-time/clj-time)
- [Documentation officielle de clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)