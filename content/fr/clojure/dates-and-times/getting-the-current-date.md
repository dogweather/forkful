---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.278123-07:00
description: "Obtenir la date actuelle en programmation est crucial pour une myriade\
  \ de raisons, y compris la journalisation, le marquage temporel des \xE9v\xE9nements\
  \ et la\u2026"
lastmod: '2024-03-13T22:44:57.293154-06:00'
model: gpt-4-0125-preview
summary: "Obtenir la date actuelle en programmation est crucial pour une myriade de\
  \ raisons, y compris la journalisation, le marquage temporel des \xE9v\xE9nements\
  \ et la planification des t\xE2ches."
title: Obtenir la date actuelle
weight: 29
---

## Quoi et pourquoi ?
Obtenir la date actuelle en programmation est crucial pour une myriade de raisons, y compris la journalisation, le marquage temporel des événements et la planification des tâches. En Clojure, un dialecte de Lisp sur la JVM, cette tâche tire parti des capacités d'interopérabilité avec Java, permettant un accès simple à la riche API Java Date-Time.

## Comment faire :

### Utiliser l’Interopérabilité avec Java
L’interopérabilité fluide de Clojure avec Java vous permet d’accéder directement à l’API Java Date-Time. Voici comment obtenir la date actuelle :

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Exemple de sortie
(get-current-date) ; "2023-04-15"
```

### Utiliser la bibliothèque clj-time
Pour une solution Clojure plus idiomatique, vous pourriez opter pour la bibliothèque `clj-time`, un wrapper autour de Joda-Time, bien que pour la plupart des nouveaux projets, l'API Java 8 Date-Time intégrée soit recommandée. Cependant, si vous préférez ou nécessitez `clj-time` :

D'abord, ajoutez `clj-time` à vos dépendances de projet. Dans votre `project.clj`, incluez :

```clojure
[clj-time "0.15.2"]
```

Ensuite, utilisez-la pour obtenir la date actuelle :

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Exemple de sortie
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Les deux méthodes fournissent des moyens rapides et efficaces pour obtenir la date actuelle en Clojure, en exploitant la puissance de la plateforme Java sous-jacente ou la commodité d'une bibliothèque spécifique à Clojure.
