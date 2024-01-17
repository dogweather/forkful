---
title:                "Rechercher et remplacer du texte"
html_title:           "Clojure: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi on le fait ?

Le remplacement de texte est une opération courante dans la programmation, qui consiste à trouver une chaîne de caractères dans un texte et à la remplacer par une autre chaîne. Les programmeurs le font souvent pour automatiser certaines tâches fastidieuses et pour effectuer des modifications en masse dans leur code.

## Comment faire :

Voici un exemple de code en Clojure pour effectuer un remplacement de texte :

```Clojure
(defn replace [text from to]
  (clojure.string/replace text from to))
```

Lorsque nous appelons cette fonction avec le texte en premier argument, la chaîne à remplacer en deuxième argument et la chaîne de remplacement en troisième argument, la fonction retourne le texte avec toutes les occurrences de la chaîne remplacées.

Par exemple, si nous avons le texte suivant :

```Clojure
(def texte "Bonjour le monde")
```

Et que nous appelons notre fonction avec ces arguments :

```Clojure
(replace texte "Bonjour" "Hello")
```

Le résultat sera :

```Clojure
"Hello le monde"
```

## Approfondissement :

Le remplacement de texte a été rendu populaire avec l'apparition des expressions régulières, un moyen puissant de rechercher et de remplacer des motifs dans un texte. Cependant, d'autres langages de programmation ont également des fonctions intégrées pour effectuer un remplacement de texte, comme la fonction ```str.replace``` de Python.

En termes d'implémentation, Clojure utilise la bibliothèque de chaînes Java pour effectuer un remplacement de texte. Cela signifie que les mêmes règles et syntaxe s'appliquent que pour la manipulation de chaînes en Java.

## Voir aussi :

- [Documentation officielle de Clojure sur les chaînes](https://clojure.org/reference/java_interop#java-string-methods)