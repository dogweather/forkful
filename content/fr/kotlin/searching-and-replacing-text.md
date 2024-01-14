---
title:                "Kotlin: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Kotlin, vous savez probablement à quel point il peut être fastidieux de parcourir manuellement votre code pour effectuer des changements de texte. Heureusement, Kotlin dispose d'une fonctionnalité pratique qui vous permet de trouver et de remplacer automatiquement du texte dans votre code.

## Comment Faire

Tout d'abord, importez la classe ```Regex``` dans votre fichier Kotlin:

```Kotlin
import kotlin.text.Regex
```

Ensuite, utilisez la fonction ```replace()``` avec un objet ```Regex``` pour rechercher et remplacer du texte dans votre chaîne de caractères, comme ceci:

```Kotlin
val texte = "Bonjour tout le monde!"
val texteModifie = texte.replace(Regex("bonjour"), "salut")

println(texteModifie)
// Sortie: Salut tout le monde!
```

Vous pouvez également utiliser la fonction ```replaceAll()``` pour remplacer toutes les occurrences de votre chaîne de recherche, ou ```replaceFirst()``` pour remplacer uniquement la première occurrence.

## Plongée Profonde

En plus de rechercher et remplacer du texte statique, vous pouvez également utiliser des expressions régulières dans votre chaîne de recherche pour des recherches plus avancées.

Par exemple, si vous souhaitez remplacer toutes les occurrences de chiffres dans une chaîne, vous pouvez utiliser l'expression régulière ```[0-9]``` comme ceci:

```Kotlin
val texte = "J'ai 5 pommes et 3 bananes."
val texteModifie = texte.replace(Regex("[0-9]"), "x")

println(texteModifie)
// Sortie: J'ai x pommes et x bananes.
```

Vous pouvez également utiliser des caractères spéciaux dans votre expression régulière pour rechercher des motifs spécifiques, comme les espaces vides, les sauts de ligne ou les caractères de ponctuation.

N'hésitez pas à explorer davantage les expressions régulières et à les utiliser dans vos recherches et remplacements de texte!

## Voir Aussi

- [Documentation Kotlin: Rechercher et remplacer du texte](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.text/-string/replace.html)
- [Tutoriel sur les expressions régulières en Kotlin](https://www.youtube.com/watch?v=S2nVSOBKwPk)
- [Documentation Regex en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)