---
title:    "Kotlin: Recherche et remplacement de texte"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur, vous savez à quel point il peut être fastidieux de trouver et remplacer du texte dans votre code. Heureusement, Kotlin offre une solution simple et efficace pour résoudre ce problème. Dans cet article, nous allons vous montrer comment utiliser la fonction de recherche et de remplacement de texte en Kotlin.

## Comment faire

Dans Kotlin, la fonction pour rechercher et remplacer du texte est `replace`. Voyons un exemple de code avec cette fonction :

```Kotlin
val phrase = "Bonjour tout le monde !"
val nouveauMot = phrase.replace("Bonjour", "Salut")

println(nouveauMot)
```
Lorsque vous exécutez ce code, la sortie sera `Salut tout le monde !`. Comme vous pouvez le voir, la fonction `replace` permet de remplacer facilement un mot par un autre dans une chaîne de caractères. Mais cette fonction ne s'arrête pas là, elle offre également la possibilité de faire des remplacements en utilisant des expressions régulières. Voyons un autre exemple :

```Kotlin
val phrase = "Aujourd'hui, c'est mercredi !"
val nouveauMot = phrase.replace("[a-z]".toRegex(), "X")

println(nouveauMot)
```
Dans cet exemple, nous utilisons une expression régulière pour remplacer toutes les lettres minuscules par la lettre `X`. La sortie sera alors `X XXHXXXX, C'XST MXXXXXXX !`.

Si vous souhaitez remplacer plusieurs mots dans la même chaîne, vous pouvez utiliser la fonction `replaceEach` qui prend en paramètre une liste de paires de mots à remplacer. Prenons un dernier exemple :

```Kotlin
val phrase = "Les chats et les chiens sont les meilleurs amis du monde."
val nouveauMot = phrase.replaceEach(
        listOf("chats" to "oiseaux", "chiens" to "poissons")
)

println(nouveauMot)
```
La sortie de ce code sera `Les oiseaux et les poissons sont les meilleurs amis du monde.`.

## Plongée en profondeur

En utilisant les expressions régulières, la fonction de recherche et de remplacement en Kotlin offre une grande flexibilité pour modifier des chaînes de caractères. Vous pouvez également utiliser la fonction `replaceFirst` pour ne remplacer que la première occurrence d'un mot dans la chaîne, ou encore utiliser les fonctions `replaceAfter` et `replaceBefore` pour cibler des mots précis avant ou après une certaine occurrence.

N'hésitez pas à explorer la documentation officielle de Kotlin pour découvrir toutes les possibilités de la fonction `replace`.

## Voir aussi

- [Documentation officielle de Kotlin sur la fonction replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/replace.html)
- [Documentation officielle de Kotlin sur les expressions régulières](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-regex.html)