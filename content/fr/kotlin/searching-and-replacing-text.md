---
title:    "Kotlin: Recherche et remplacement de texte"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Il peut être nécessaire de rechercher et remplacer du texte dans votre code Kotlin pour plusieurs raisons. Par exemple, vous pourriez vouloir corriger une faute de frappe ou mettre à jour une variable dans plusieurs endroits de votre projet.

# Comment faire

Pour rechercher et remplacer du texte dans un code Kotlin, vous pouvez utiliser la fonction `replace` de la classe `String`. Voici un exemple de code qui remplace toutes les occurrences de la chaîne "bonjour" par "salut":

```Kotlin
val text = "Bonjour tout le monde!"
val newText = text.replace("bonjour", "salut")
println(newText)

// Output: Salut tout le monde!
```

Vous pouvez également utiliser des expressions régulières pour effectuer une recherche et un remplacement plus avancés. Par exemple, le code suivant remplace toutes les lettres "a" ou "A" par le symbole "#":

```Kotlin
val text = "Ceci est un exemple de texte."
val newText = text.replace(Regex("[aA]"), "#")
println(newText)

// Output: C#ci est un ex#mple de texte.
```

# Plongée en profondeur

En utilisant des expressions régulières, vous pouvez effectuer des recherches et des remplacements encore plus complexes, comme chercher et remplacer des mots qui commencent par une certaine lettre ou qui contiennent un certain motif. Il est également possible de spécifier le nombre maximum d'occurrences à remplacer ou d'utiliser des fonctions lambda pour personnaliser votre recherche et votre remplacement.

Veuillez noter que la fonction `replace` n'est pas limitée aux chaînes de caractères, elle peut également être utilisée avec des listes, des tableaux ou d'autres types de collections. Pour plus d'informations sur les différentes options de `replace`, consultez la [documentation officielle de Kotlin](https://kotlinlang.org/docs/reference/strings.html#string-replacements).

# Voir aussi

- [Documentation officielle de Kotlin sur les chaînes de caractères](https://kotlinlang.org/docs/reference/strings.html)
- [Guide sur les expressions régulières en Kotlin](https://www.baeldung.com/kotlin-regular-expressions)