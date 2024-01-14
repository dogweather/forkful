---
title:    "Clojure: Obtenir la date actuelle"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de connaître la date actuelle dans vos programmes Clojure? Peut-être que vous voulez enregistrer la date et l'heure d'un événement particulier, ou peut-être que vous voulez simplement afficher la date actuelle dans votre application. Quelle que soit la raison, il est important de savoir comment obtenir la date actuelle en utilisant Clojure.

## Comment faire

Voici quelques exemples de code pour obtenir la date actuelle en Clojure:

```Clojure
(+ 2 2) ;=> 4
```

```Clojure
(print "La date actuelle est: ")
(print (java.util.Date.))
```
Sortie:
```
La date actuelle est: Sat Dec 05 20:30:00 EST 2020
```

Vous pouvez également utiliser la fonction ```now``` du package ```clj-time``` pour obtenir la date et l'heure actuelles en utilisant une syntaxe plus conviviale:

```Clojure
(ns mon-projet.core
    (:require [clj-time.core :as t]))

(print "La date et l'heure actuelles sont: ")
(print (t/now))
```
Sortie:
```
La date et l'heure actuelles sont: 2020-12-05T20:30:00.000-05:00
```

## Plongez plus profondément

Pour obtenir une date spécifique (par exemple, la date d'aujourd'hui dans un fuseau horaire spécifique), vous pouvez utiliser la fonction ```now-in``` du package ```clj-time```. Vous pouvez également formater la date selon vos besoins en utilisant la fonction ```format``` du même package.

Il est également important de noter que la date et l'heure actuelles sont basées sur le fuseau horaire du système. Si vous souhaitez obtenir la date et l'heure dans un fuseau horaire spécifique, vous devrez passer par des étapes supplémentaires pour définir le fuseau horaire.

## Voir aussi

- [Documentation officielle de Clojure](https://clojure.org/)
- [Référence de la fonction now du package clj-time](https://clj-time.github.io/clj-time/doc/clj-time.core.html#var-now)
- [Guide de démarrage rapide de Clojure](https://clojure.org/guides/getting_started)