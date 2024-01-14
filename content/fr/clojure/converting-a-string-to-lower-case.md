---
title:    "Clojure: Conversion d'une chaîne en minuscules"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Bien que cela puisse sembler simple, convertir une chaîne de caractères en minuscules est une tâche courante dans de nombreux projets de développement. Cela peut être utile pour normaliser les chaînes de caractères et les rendre plus facilement comparables ou pour les manipuler de différentes manières. Dans cet article, nous allons explorer comment le faire en utilisant Clojure.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Clojure, nous pouvons utiliser la fonction `clojure.string/lower-case`. Voici un exemple de code avec une chaîne de caractères en entrée et la chaîne de caractères en minuscules en sortie :

```Clojure
(def s "HELLO")
(clojure.string/lower-case s)
;; output: "hello"
```

Nous pouvons également utiliser la même fonction avec des chaînes de caractères plus longues :

```Clojure
(def s "THIS IS A LONG STRING TO BE CONVERTED TO LOWER CASE")
(clojure.string/lower-case s)
;; output: "this is a long string to be converted to lower case"
```

Il est important de noter que cette fonction est sensible à la casse et ne convertira que les lettres majuscules en minuscules. Les caractères spéciaux, les chiffres et les espaces ne seront pas modifiés.

```Clojure
(def s "HeLlO, 123!")
(clojure.string/lower-case s)
;; output: "hello, 123!"
```

## Plongée en profondeur

Maintenant que nous avons vu comment utiliser la fonction `clojure.string/lower-case`, il est intéressant de savoir comment elle fonctionne sous le capot. Cette fonction utilise la norme Unicode pour convertir les caractères en minuscules. Cela signifie que les caractères spéciaux et les lettres accentuées seront également convertis correctement.

Par exemple, la lettre "Ê" sera convertie en "ê" et la lettre "Ñ" sera convertie en "ñ". Cela peut être utile lorsque nous travaillons avec du texte dans différentes langues.

De plus, il est possible de spécifier un paramètre `locale` pour la fonction `lower-case`, qui peut être utilisé pour convertir les caractères selon les règles spécifiques à une langue ou une région.

Enfin, il est important de noter que la fonction `lower-case` est préférable à l'utilisation de la fonction `string/lower-case` directement sur une chaîne de caractères. Cela est dû au fait que `clojure.string/lower-case` gère les chaînes de caractères de manière plus efficace en utilisant des calculs de codepoints plutôt que de caractères individuels.

## Voir aussi

- Documentation officielle de la fonction `clojure.string/lower-case` : https://clojuredocs.org/clojure.string/lower-case
- Utilisation des locales en Clojure : https://gist.github.com/jinjor/1562134
- Guide complet sur la manipulation des chaînes de caractères en Clojure : https://clojuredocs.org/clojure.string

Merci d'avoir lu cet article sur la conversion de chaînes de caractères en minuscules en Clojure ! J'espère que vous avez trouvé cela utile et que cela vous aidera dans vos projets de développement. N'hésitez pas à explorer davantage les fonctions de manipulation de chaînes de caractères en Clojure pour améliorer vos compétences en programmation.