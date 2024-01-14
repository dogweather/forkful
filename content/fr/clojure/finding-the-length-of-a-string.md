---
title:                "Clojure: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

L'une des tâches les plus courantes lors de la programmation est de trouver la longueur d'une chaîne de caractères. Que vous soyez un débutant ou un développeur chevronné, vous devrez souvent effectuer cette opération dans vos projets. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne en utilisant Clojure.

# Comment faire

La première étape pour trouver la longueur d'une chaîne en Clojure est de connaître la syntaxe de base pour les chaînes de caractères. En Clojure, une chaîne est représentée entre guillemets (") et peut contenir des caractères, des nombres, des espaces et des symboles spéciaux.

Voici un exemple de déclaration de chaîne en Clojure:

```Clojure
(def my-string "Bonjour le monde")
```

Maintenant que nous avons une chaîne définie, nous pouvons utiliser la fonction `count` pour trouver sa longueur. Cette fonction renvoie simplement le nombre de caractères dans une chaîne donnée.

Voici un exemple de code complet pour trouver la longueur d'une chaîne en Clojure:

```Clojure
(def my-string "Bonjour le monde")
(count my-string) ; renvoie 17
```

Comme vous pouvez le voir, c'est assez simple et direct. Vous pouvez également utiliser cette méthode pour trouver la longueur d'une chaîne stockée dans une variable ou directement en utilisant la chaîne elle-même.

# Plongée en profondeur

Il est important de comprendre comment Clojure traite les chaînes pour comprendre pourquoi la fonction `count` fonctionne pour trouver la longueur d'une chaîne. En Clojure, une chaîne est en fait une séquence, ce qui signifie qu'elle est composée de plusieurs éléments indexés. La fonction `count` compte tout simplement le nombre d'éléments dans cette séquence, nous donnant ainsi la longueur de la chaîne.

Une autre méthode pour trouver la longueur d'une chaîne en utilisant Clojure est d'utiliser la fonction `str` pour la concaténer avec une chaîne vide, puis d'utiliser `count` comme précédemment.

Voici un exemple de code utilisant cette méthode alternative:

```Clojure
(def my-string "Bonjour le monde")
(count (str my-string "")) ; renvoie 17
```

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur les chaînes en Clojure:

- Documentation officielle sur les chaînes en Clojure : https://clojure.org/reference/strings
- Un tutoriel vidéo expliquant les bases des chaînes en Clojure : https://www.youtube.com/watch?v=eRbRD5lvGUc

Merci d'avoir lu cet article sur la façon de trouver la longueur d'une chaîne en utilisant Clojure. Nous espérons que cela vous sera utile dans vos futurs projets de programmation !