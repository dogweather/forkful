---
title:                "Clojure: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

La manipulation de chaînes de caractères est souvent un aspect important dans la programmation, et l'extraction de sous-chaînes est une tâche courante lors du traitement de données. En apprenant à extraire des sous-chaînes en utilisant Clojure, vous pourrez facilement manipuler et traiter des données de manière plus efficace.

# Comment Faire

Nous allons utiliser la fonction "subs" pour extraire des sous-chaînes dans Clojure. Cette fonction prend deux paramètres: la chaîne de caractères à extraire et les indices de début et de fin de la sous-chaîne souhaitée. Voici un exemple de code et de sortie :

```
(def s "Bonjour tout le monde!")

(subs s 8 15)
```
Résultat: `"tout le"`

La fonction "subs" peut également être utilisée pour extraire une sous-chaîne à partir d'un index donné jusqu'à la fin de la chaîne. Par exemple:

```
(def s "Bonjour tout le monde!")

(subs s 8)
```
Résultat: `"tout le monde!"`

Vous pouvez également utiliser des nombres négatifs pour indiquer un index à partir de la fin de la chaîne. Par exemple:

```
(def s "Bonjour tout le monde!")

(subs s -7 -1)
```
Résultat: `"monde!"`

# Plongée Profonde

La fonction "subs" utilise l'indice de début inclusif et l'indice de fin exclusif. Cela signifie que l'indice de fin ne sera pas inclus dans la sous-chaîne extraite. Par exemple:

```
(def s "Bonjour tout le monde!")

(subs s 0 7)
```
Résultat: `"Bonjour"`

Dans cet exemple, l'indice de début est 0 et l'indice de fin est 7. Cela signifie que seuls les caractères aux indices 0 à 6 (inclus) seront inclus dans la sous-chaîne.

De plus, si vous utilisez un indice négatif pour l'indice de fin, celui-ci sera compté à partir de la fin de la chaîne. Par exemple:

```
(def s "Bonjour tout le monde!")

(subs s 0 -1)
```
Résultat: `"Bonjour tout le monde!"`

# Voir Aussi

- [Documentation officielle de Clojure sur la fonction "subs"](https://clojuredocs.org/clojure.core/subs)
- [Guide complet sur les sous-chaînes en Clojure](https://www.baeldung.com/string-subsets-in-clojure)