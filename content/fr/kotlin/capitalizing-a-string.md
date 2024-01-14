---
title:    "Kotlin: Mise en majuscule d'une chaîne de caractères"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères peut être utile dans de nombreuses situations. Cela peut permettre d'homogénéiser les données, améliorer la lisibilité du texte et faciliter la recherche d'informations spécifiques dans une chaîne.

## Comment faire

Voici un exemple de code en Kotlin pour capitaliser une chaîne de caractères :

```Kotlin
val mot = "bonjour"
val motCapital = mot.capitalize()

println(motCapital)

// Output : Bonjour
```

Vous pouvez également utiliser la méthode `toUpperCase()` pour capitaliser toutes les lettres d'une chaîne, ou `toLowerCase()` pour les mettre en minuscules.

```Kotlin
val phrase = "c'est une phrase"
val phraseCapital = phrase.toUpperCase()

println(phraseCapital)

// Output : C'EST UNE PHRASE
```

## Plongée en profondeur

La capitalisation dans les langages de programmation peut être réalisée de différentes manières. En Kotlin, la méthode `capitalize()` utilise le premier caractère de la chaîne pour le mettre en majuscule, sans affecter les autres caractères. Pour obtenir un résultat différent en termes de capitalisation, il est possible d'utiliser la méthode `replace()` pour remplacer un caractère spécifique par sa version majuscule ou minuscule.

## Voir aussi

- [Documentation officielle de Kotlin sur la manipulation de chaînes](https://kotlinlang.org/docs/reference/strings.html)
- [Guide complet sur la capitalisation en Kotlin](https://www.baeldung.com/kotlin/string-capitalization)
- [Exemples pratiques de manipulation de chaînes en Kotlin](https://www.geeksforgeeks.org/kotlin-string-operations/)