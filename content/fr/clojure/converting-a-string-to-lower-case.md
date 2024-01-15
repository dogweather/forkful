---
title:                "Convertissement d'une chaîne en minuscules"
html_title:           "Clojure: Convertissement d'une chaîne en minuscules"
simple_title:         "Convertissement d'une chaîne en minuscules"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Clojure, vous pourriez avoir besoin de les convertir en minuscules pour diverses raisons. Par exemple, si vous comparez des chaînes de caractères, la casse peut affecter le résultat de votre comparaison. La conversion en minuscules peut également être utile si vous avez besoin de manipuler des données entrées par un utilisateur ou provenant d'une source externe et que vous souhaitez normaliser le format.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Clojure, vous pouvez utiliser la fonction `lower-case`. Par exemple:

```Clojure
(lower-case "Hello World") ; "hello world"
```

Le résultat sera une nouvelle chaîne de caractères en minuscules. La fonction `lower-case` peut prendre un ou plusieurs arguments et fonctionner avec des chaînes de caractères, des caractères, des symboles ou des séquences de caractères. Si vous ne souhaitez pas créer une nouvelle chaîne de caractères mais plutôt modifier la chaîne d'origine, vous pouvez utiliser la fonction `clojure.string/lower-case`:

```Clojure
(require '[clojure.string :as str])
(def s "Hello World")
(str/lower-case! s)
s ; "hello world"
```

La fonction `lower-case!` modifie directement la chaîne de caractères passée en argument. Cela peut être pratique si vous travaillez avec des données mutables, mais soyez prudent car cela peut avoir des effets secondaires.

## Plongée en profondeur

La fonction `lower-case` utilise les règles de conversion Unicode pour passer une chaîne de caractères en minuscules. Cela signifie que les caractères accentués ou spéciaux seront également convertis correctement. Par exemple:

```Clojure
(lower-case "Éléphant") ; "éléphant"
```

De plus, si vous travaillez avec des caractères ASCII, la fonction `lower-case` utilisera les règles de conversion ASCII, ce qui peut être utile pour un traitement plus rapide et plus spécifique.

## Voir aussi

- Documentation officielle de Clojure sur la fonction `lower-case`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case
- Article sur la manipulation de chaînes de caractères en Clojure: https://purelyfunctional.tv/guide/clojure-strings/