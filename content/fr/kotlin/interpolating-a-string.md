---
title:                "Interpoler une chaîne de caractères."
html_title:           "Kotlin: Interpoler une chaîne de caractères."
simple_title:         "Interpoler une chaîne de caractères."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

#### Qu'est-ce que c'est et pourquoi le faisons-nous?

L'interpolation de chaîne est le processus de créer une chaîne en insérant dynamiquement des valeurs dans une chaîne existante. Les programmeurs utilisent cela pour simplifier et rendre plus lisible le code en évitant de concaténer plusieurs chaînes ensemble.

#### Comment faire:

Exemples de code et résultats:

```Kotlin
val name = "Jean"
val age = 25
val sentence = "Bonjour, je m'appelle $name et j'ai $age ans."
println(sentence)
```
Résultat: Bonjour, je m'appelle Jean et j'ai 25 ans.

L'utilisation de l'interpolation de chaîne en Kotlin est simple et elle peut être utilisée à la fois avec des variables et des expressions. Il suffit d'ajouter le symbole '$' suivi du nom de la variable ou de l'expression à l'endroit souhaité dans la chaîne. De plus, il est également possible d'utiliser la syntaxe de formatage pour spécifier le nombre de chiffres significatifs à utiliser pour les valeurs numériques.

```Kotlin
val price = 100.50
val quantity = 3
val total = price * quantity
println("Le coût total est de \$%.2f pour $quantity articles.".format(total))
```

Résultat: Le coût total est de $301.50 pour 3 articles.

#### Approfondissement:

L'interpolation de chaîne est une technique couramment utilisée dans les langages de programmation modernes pour améliorer la lisibilité du code. Elle a été popularisée par le langage de programmation Perl dans les années 1980 et est maintenant utilisée dans d'autres langages tels que Python, Ruby et bien sûr Kotlin.

Une alternative à l'interpolation de chaîne est l'utilisation de la concaténation de chaînes, qui consiste à combiner plusieurs chaînes ensemble en utilisant un opérateur de concaténation tel que '+' ou '.concat()'. Cependant, cela peut devenir fastidieux et difficile à lire lorsque plusieurs valeurs doivent être insérées dans une chaîne.

En ce qui concerne l'implémentation de l'interpolation de chaîne, elle est gérée par le compilateur Kotlin, ce qui signifie que le code final sera optimisé pour une performance maximale.

#### Voir aussi:

Pour en savoir plus sur l'interpolation de chaîne en Kotlin, vous pouvez consulter la documentation officielle de Kotlin sur [les chaînes et les caractères](https://kotlinlang.org/docs/reference/basic-types.html#strings-and-characters). Vous pouvez également jeter un coup d'œil à [cet article](https://www.baeldung.com/kotlin-string-interpolation) pour plus d'exemples et d'astuces sur l'utilisation de l'interpolation de chaîne en Kotlin.