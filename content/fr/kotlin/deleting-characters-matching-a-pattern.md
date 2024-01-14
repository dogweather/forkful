---
title:                "Kotlin: Suppression de caractères correspondants à un modèle"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez pourquoi vous devriez vous intéresser à supprimer des caractères correspondant à un motif en Kotlin ? Vous êtes au bon endroit ! La suppression de caractères correspondant à un motif est un outil utile à connaître pour effectuer des modifications de chaînes de manière efficace et rapide.

## Comment faire

Pour supprimer des caractères correspondant à un motif en Kotlin, il existe plusieurs méthodes. Voici deux façons d'y parvenir :

1. Utiliser la méthode `replace()` sur la chaîne et passer le motif à supprimer et une chaîne vide en tant que paramètres. En voici un exemple :

```Kotlin
val originalString = "Bonjour le monde !"
val modifiedString = originalString.replace("le monde", "")
println(modifiedString)
```

Résultat :

```Kotlin
Bonjour  !
```

2. Utiliser la méthode `replace(regex, "")` en passant un motif regex (expression régulière) en tant que premier paramètre et également une chaîne vide en tant que deuxième paramètre. Voici un exemple :

```Kotlin
val originalString = "Codez avec Kotlin !"
val modifiedString = originalString.replace(Regex("[ae]"), "")
println(modifiedString)
```

Résultat :

```Kotlin
Codz vc Ktlin !
```

## Plongée en profondeur

Maintenant que vous savez comment supprimer des caractères correspondant à un motif en Kotlin, plongeons plus en détail dans la méthode `replace()` et la classe `Regex`. La méthode `replace()` renvoie une nouvelle chaîne avec les caractères remplacés selon le motif spécifié. La classe `Regex` est utilisée pour représenter une expression régulière et est souvent utilisée pour effectuer des opérations de correspondance et de remplacement complexes sur des chaînes.

## Voir aussi

- [Documentation Kotlin sur replace()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Documentation Kotlin sur Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)