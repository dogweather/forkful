---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Clojure: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Sérieusement, pourquoi vouloir capitaliser une chaîne de caractères en Clojure ? Honnêtement, il n'y a pas de réponse claire et précise à cette question. Certaines personnes peuvent le faire pour des raisons esthétiques ou pour suivre les conventions de codage, tandis que d'autres peuvent le faire pour des raisons pratiques. Quelle que soit la raison, capitaliser une chaîne de caractères en Clojure est assez simple et peut vous aider à écrire du code plus propre et plus lisible.

## Comment faire

Pour commencer, vous devez comprendre ce qu'est une chaîne de caractères en Clojure. En bref, une chaîne de caractères est une séquence de caractères entourée de guillemets doubles. Par exemple, "Bonjour le monde !" est une chaîne de caractères valide en Clojure.

Maintenant que nous savons ce qu'est une chaîne de caractères, voici comment capitaliser une chaîne de caractères en Clojure :

```
(defn capitalize-string [str]
  (str/upper-case str))
```

Dans cet exemple, nous avons défini une fonction appelée "capitalize-string" qui prend une chaîne de caractères en entrée et utilise la fonction "upper-case" du namespace "str" pour la convertir en majuscules. Nous pouvons ensuite appeler cette fonction avec une chaîne de caractères et voir le résultat :

```
(capitalize-string "bonjour le monde") ;;= "BONJOUR LE MONDE"
```

Comme vous pouvez le constater, notre chaîne de caractères a bien été capitalisée en utilisant cette méthode.

## Plongée plus profonde

Maintenant que nous avons vu comment capitaliser une chaîne de caractères en Clojure, voici quelques points importants à retenir :

- Il existe d'autres fonctions de manipulation de chaînes de caractères disponibles dans le namespace "str" tels que "lower-case", "capitalize" et "title-case".

- Vous pouvez également utiliser la fonction "clojure.string/capitalize" pour capitaliser uniquement la première lettre d'une chaîne de caractères.

- La fonction "str/upper-case" convertit tous les caractères en majuscules, y compris les caractères spéciaux tels que les accents et les caractères unicode.

- Rappelez-vous que les chaînes de caractères sont immuables en Clojure, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Toute fonction de manipulation de chaîne de caractères renverra une nouvelle chaîne de caractères plutôt que de modifier l'originale.

## Voir aussi

Maintenant que vous savez comment capitaliser une chaîne de caractères en Clojure, voici quelques liens utiles pour en apprendre plus sur la manipulation de chaînes de caractères :

- [La documentation officielle de la bibliothèque standard de Clojure](https://clojure.org/reference/strings)
- [Un tutoriel sur les fonctions de manipulation de chaînes de caractères en Clojure](https://purelyfunctional.tv/guide/clojure-strings/)
- [Un article approfondi sur l'immutabilité en Clojure](https://blog.klipse.tech/clojure/2016/07/15/clojure-strings-1.html)