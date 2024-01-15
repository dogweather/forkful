---
title:                "Convertir une chaîne en minuscules"
html_title:           "Kotlin: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Convertir une chaîne de caractères en minuscules est une tâche courante dans la programmation, notamment pour faciliter la comparaison entre deux chaînes ou pour formater des données en minuscules. En utilisant Kotlin, cette manipulation peut être réalisée de manière simple et efficace.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant Kotlin, il suffit d'appeler la fonction `toLowerCase()` sur l'objet String correspondant. Voici un exemple de code et sa sortie :

```Kotlin
val phrase = "Je Suis UNE CHAÎNE"
val phraseEnMinuscules = phrase.toLowerCase()
println(phraseEnMinuscules)
```

Output: je suis une chaîne

Comme vous pouvez le constater, la fonction `toLowerCase()` transforme toutes les lettres en minuscules, y compris les caractères spéciaux comme les accents. Si vous souhaitez conserver les caractères spéciaux, vous pouvez utiliser la fonction `toLocaleLowerCase()` à la place.

Si vous avez besoin de convertir des chaînes de caractères en minuscules dans une boucle ou pour une grande quantité de données, il est recommandé d'utiliser la fonction `toLowerCase(Locale)` en spécifiant la locale appropriée. Par exemple :

```Kotlin
val phrase = "HËLLO"
val locale = Locale("fr")
val phraseEnMinuscules = phrase.toLowerCase(locale)
println(phraseEnMinuscules)
```

Output: hëllo

## Deep Dive

La fonction `toLowerCase()` utilise le standard Unicode pour la conversion en minuscules. Ce standard inclut également des règles spécifiques pour certaines langues et caractères, permettant ainsi de gérer efficacement les cas de langues avec des caractères spéciaux.

Il est important de noter que la fonction `toLowerCase()` retourne une nouvelle chaîne de caractères avec les modifications, et ne modifie pas directement la chaîne originale. Pour modifier la chaîne d'origine, il est nécessaire de réassigner la valeur après la conversion, comme dans l'exemple suivant :

```Kotlin
var phrase = "J'AIME Le KOTLIN"
phrase = phrase.toLowerCase()

println(phrase) // j'aime le kotlin 
```

## Voir aussi

- Documentation officielle de Kotlin sur les chaînes de caractères : https://kotlinlang.org/docs/reference/basic-types.html#strings
- Conversion de chaînes de caractères en majuscules avec Kotlin : https://developer85.com/fr/programming/2020/03/12/kotlin-convertir-chaine-caractere-majuscules.html 
- Tutoriel sur la manipulation des chaînes de caractères en Kotlin : https://blog.arungeorge.in/kotlin-string-manipulation/