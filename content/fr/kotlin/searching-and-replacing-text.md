---
title:                "Recherche et remplacement de texte"
html_title:           "Kotlin: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi 

Vous avez peut-être déjà rencontré cette situation frustrante : vous avez un long document rempli de fautes d'orthographe ou de mots mal écrits et vous ne savez pas comment les corriger tous en une seule fois. C'est là que la fonction de recherche et de remplacement de texte entre en jeu. Avec Kotlin, vous pouvez facilement effectuer ces modifications en quelques lignes de code, ce qui vous fait gagner un temps précieux !

## Comment faire 

Voici comment utiliser la fonction de recherche et de remplacement de texte en Kotlin avec quelques exemples de code et leur résultat :

```
val str = "Bonjour, je suis un texte de test."
println(str.replace("Bonjour", "Salut"))
```

Résultat : *Salut, je suis un texte de test.*

```
val str = "Ceci est une phrase à modifier."
println(str.replace("modifier", "changer"))
```

Résultat : *Ceci est une phrase à changer.*

En utilisant la fonction `.replace`, vous pouvez spécifier le texte que vous souhaitez remplacer ainsi que le nouveau texte à insérer à sa place. Il est également possible d'utiliser des expressions régulières pour effectuer des remplacements plus complexes. Voici un exemple :

```
val str = "12/03/2020"
println(str.replace(Regex("[0-9]{2}/"), "03/"))
```

Résultat : *03/03/2020*

Dans cet exemple, nous avons utilisé une expression régulière pour rechercher les deux premiers chiffres correspondant au jour dans la date et les avons remplacés par "03/" afin d'obtenir une date au mois de mars. Vous pouvez également utiliser la fonction `.replaceFirst` si vous souhaitez ne remplacer que la première occurrence plutôt que toutes les occurences. 

## Plongée en profondeur 

Maintenant que vous savez comment utiliser la fonction de recherche et de remplacement de texte en Kotlin, voici quelques points à prendre en compte lors de son utilisation :

- Kotlin prend en charge l'utilisation de chaînes de caractères multilignes, ce qui peut être utile pour effectuer des remplacements sur des textes longs contenant plusieurs lignes.
- Les expressions régulières sont un outil puissant pour effectuer des recherches et des remplacements de texte, mais elles peuvent être complexes à comprendre et à mettre en œuvre. Il est conseillé de se familiariser avec leur utilisation avant de les utiliser dans le code.
- Il est possible de remplacer un texte par une chaîne vide en passant `""` comme paramètre du `.replace`.

## Voir aussi 

- [Documentation officielle de Kotlin sur la fonction replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Documentation sur les expressions régulières en Kotlin](https://kotlinlang.org/docs/reference/regexp.html)
- [Tutoriel sur l'utilisation des expressions régulières en Kotlin](https://www.tutorialkart.com/kotlin/using-regular-expressions-in-kotlin/)

Maintenant que vous savez comment utiliser la fonction de recherche et de remplacement de texte en Kotlin, vous pouvez facilement effectuer des modifications sur vos documents ou fichiers de code. Profitez de cette fonction pour gagner du temps et améliorer la qualité de vos textes !