---
title:                "Utiliser les expressions régulières"
html_title:           "Kotlin: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur chevronné, vous avez sûrement déjà entendu parler des expressions régulières (plus communément appelées regex). Et si vous êtes nouveau dans le domaine, vous êtes sur le point de découvrir leur potentiel étonnant. Les regex sont des outils puissants pour la manipulation et la vérification de chaînes de caractères. Que ce soit pour valider des données d'utilisateur dans un formulaire ou pour extraire des informations d'un texte volumineux, les regex peuvent être utilisées dans une variété de scénarios de programmation.

## Comment faire

Pour utiliser des expressions régulières en Kotlin, vous devez importer le module regex dans votre code. Voici un exemple de code pour rechercher et imprimer toutes les occurrences de "chat" dans une chaîne de caractères :

```Kotlin
import kotlin.text.regex.*
val input = "J'ai vu un joli chat noir dans la rue aujourd'hui"
val regex = Regex("chat")
val matches = regex.findAll(input)
for (match in matches) {
    println(match.value)
}
```

Cet exemple utilise la méthode `findAll()` pour trouver toutes les occurrences de "chat" dans la chaîne `input`. La méthode renvoie un objet de type `MatchResult` qui contient les informations sur la correspondance. Vous pouvez ensuite utiliser `match.value` pour accéder à la chaîne correspondante.

Vous pouvez également utiliser des expressions régulières pour valider un format d'email, un numéro de téléphone ou même un mot de passe. Voici un exemple de validation d'un numéro de téléphone américain :

```Kotlin
val input = "555-1234"
val regex = Regex("\\d{3}-\\d{4}")
if (regex.matches(input)) {
    println("Numéro de téléphone valide!")
} else {
    println("Veuillez entrer un numéro de téléphone valide")
}
```

Dans cet exemple, la méthode `matches()` est utilisée pour vérifier si la chaîne de caractères `input` correspond au format spécifié dans la regex.

## Plongée en profondeur

Les regex ne sont pas limitées à des recherches simples de chaînes de caractères. Vous pouvez utiliser des expressions régulières pour capturer des groupes de caractères spécifiques, utiliser des caractères spéciaux pour des correspondances plus complexes et même utiliser des modificateurs pour rendre les regex sensibles à la casse ou aux espaces. Avec un peu d'expérience, vous pourrez résoudre des problèmes de manipulation de chaînes de caractères rapidement et efficacement grâce à votre connaissance des regex.

Cependant, il est important de noter que les expressions régulières peuvent être difficiles à comprendre pour les débutants. Si vous êtes nouveau dans le monde des regex, il peut être utile de suivre des tutoriels et de pratiquer régulièrement pour maîtriser cet outil puissant.

## Voir aussi

- [Documentation officielle de Kotlin sur les expressions régulières](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutoriel interactif sur les expressions régulières en Kotlin](https://play.kotlinlang.org/hands-on/regular-expressions)