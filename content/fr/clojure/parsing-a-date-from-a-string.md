---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "Clojure: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Parsing une date à partir d'une chaîne de caractères, c'est le processus de prendre une date sous forme de texte et de la convertir en un format que l'ordinateur peut comprendre. Les programmeurs font cela pour pouvoir utiliser et manipuler les dates dans leurs programmes de manière plus efficace.

## Comment:
Ci-dessous, vous trouverez un exemple de code montrant comment parser une date à partir d'une chaîne de caractères en Clojure.

```Clojure
(require '[java-time :as jt])

(defn parse-date [str]
  ;; utilisez la méthode parse de java-time pour convertir la chaîne en une date
  (let [date (jt/parse (jt/of-pattern "dd/MM/yyyy") str)]
    (println date)))

(parse-date "23/09/2021")
```
Output: #object["java.time.LocalDate" "2021-09-23"]

Dans cet exemple, nous utilisons la bibliothèque java-time pour accéder à la méthode de parsing. Nous spécifions également le format de la date dans laquelle la chaîne est écrite, dans ce cas "dd/MM/yyyy". La méthode parse renvoie un objet date, que nous pouvons ensuite utiliser dans notre programme.

## Plongée en profondeur:
Le parsing de dates à partir de chaînes de caractères est un problème courant en informatique, et il existe de nombreuses bibliothèques et méthodes différentes pour le résoudre. L'utilisation de java-time est l'une des options les plus courantes en Clojure, mais d'autres bibliothèques telles que clj-time sont également populaires.

L'utilisation de bibliothèques pour le parsing de dates est une solution plus robuste que l'écriture de votre propre algorithme de parsing à partir de zéro. Les bibliothèques ont souvent des fonctionnalités avancées telles que la prise en charge de différents formats de date et la gestion des fuseaux horaires.

L'implémentation exacte du parsing de dates peut varier en fonction de la bibliothèque utilisée, mais elle implique généralement de définir un modèle de pattern pour représenter le format de la date prévu et de le fournir à une méthode de parsing.

## Voir aussi:
- [Documentation officielle de java-time](https://clojure.org/api/java.time)
- [Documentation officielle de clj-time](https://github.com/clj-time/clj-time)