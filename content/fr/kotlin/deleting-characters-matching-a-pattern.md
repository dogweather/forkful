---
title:    "Kotlin: Supprimer les caractères correspondant à un modèle"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#Pourquoi

Supprimer des caractères correspondants à un motif peut être utile lorsqu'il s'agit de nettoyer ou de filtrer des données. Cela peut également être utilisé pour formater une chaîne de caractères selon un modèle spécifique.

##Comment Faire

```Kotlin
val string = "Bonjour, mon nom est Jean."
val pattern = """[a-zA-Z]+"""
val result = string.replace(Regex(pattern), "")
print(result)
```
**Output:** , .

Dans cet exemple, nous utilisons la méthode `replace` pour supprimer tous les caractères correspondant au motif `[a-zA-Z]+`, qui représente toutes les lettres de l'alphabet en majuscules et minuscules. Cela nous donne une chaîne de caractères résultante qui ne contient que des espaces et des signes de ponctuation. 

Il est important de noter que la méthode `replace` retourne une nouvelle chaîne de caractères et ne modifie pas la chaîne d'origine. Si vous souhaitez modifier la chaîne d'origine, vous pouvez utiliser la méthode `replace` sur une variable mutable de type `StringBuilder`.

##Plongée Profonde

Maintenant, allons un peu plus en profondeur sur la façon dont cela fonctionne réellement. La méthode `replace` prend deux paramètres : le premier est le motif Regex que nous voulons rechercher et remplacer, et le deuxième est la valeur de remplacement. Dans notre exemple, nous utilisons une chaîne vide comme valeur de remplacement, ce qui signifie que tout ce qui correspond au motif sera remplacé par une chaîne vide.

Il est également possible d'utiliser des groupes de capture pour personnaliser la façon dont la chaîne est remplacée. Par exemple, si nous voulons remplacer tous les noms dans une chaîne par la chaîne `"John Doe"`, nous pouvons utiliser le groupe de capture `$0` qui correspond à la partie de la chaîne qui a été trouvée par le motif.

```Kotlin
val string = "Bonjour, mon nom est Jean."
val pattern = """([a-zA-Z]+) ([a-zA-Z]+) ([a-zA-Z]+)"""
val result = string.replace(Regex(pattern), "$0 Doe")
print(result)
```

**Output:** Bonjour, mon nom est John Doe.

Le motif que nous utilisons ici correspond à trois groupes de lettres formant un nom complet. En utilisant `$0` comme valeur de remplacement, nous remplaçons tout le nom par `John Doe`. Nous pouvons également utiliser d'autres groupes de capture dans notre valeur de remplacement, tels que `$1`, `$2`, etc., pour faire référence à des parties spécifiques du motif.

#Voir Aussi

- [Référence de la classe Regex dans la documentation de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/) 
- [Tutoriel sur l'utilisation des Regex en Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm)