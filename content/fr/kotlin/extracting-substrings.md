---
title:                "Extraction de sous-chaînes"
html_title:           "Kotlin: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères dans vos projets Kotlin, il est fort probable que vous ayez besoin d'extraire des sous-chaînes à un moment donné. Cela peut être utile lorsque vous souhaitez récupérer une partie spécifique d'une chaîne pour effectuer des opérations ultérieures, telles que la manipulation de données ou la vérification de certaines conditions. Dans cet article, nous allons vous montrer comment extraire des sous-chaînes en utilisant Kotlin de manière efficace et pratique.

## Comment faire

Pour extraire une sous-chaîne d'une chaîne principale en Kotlin, nous allons utiliser la méthode ```substring()``` qui est déjà intégrée dans le langage. Cette méthode prend deux paramètres : la position de départ et la position de fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple simple qui illustre son utilisation :

```Kotlin
val str = "Bonjour le monde"
val sousChaine = str.substring(8, 11)

println(sousChaine) // Cela affichera "mon"
```

Comme vous pouvez le voir, nous avons spécifié les positions 8 et 11, qui sont respectivement la première et la dernière lettre de la sous-chaîne "mon". La méthode ```substring()``` renvoie alors une nouvelle chaîne contenant cette sous-chaîne spécifique. Vous pouvez également utiliser seulement la position de départ et omettre la position de fin, auquel cas la méthode extraira la sous-chaîne à partir de la position donnée jusqu'à la fin de la chaîne principale.

En plus de la méthode ```substring()```, Kotlin offre également la possibilité d'utiliser l'opérateur de tranche ```..``` pour extraire des sous-chaînes. Voici un exemple utilisant la même chaîne principale :

```Kotlin
val sousChaine = str[8..11]

println(sousChaine) // Cela affichera "mon"
```

Ici, nous avons utilisé l'opérateur ```..``` pour spécifier les positions de la sous-chaîne que nous voulons extraire. Cela fonctionne de la même manière que la méthode ```substring()```, mais peut être plus pratique à utiliser dans certaines situations.

## Deep Dive

Il est important de noter que les indices dans Kotlin commencent à partir de 0, donc la première lettre d'une chaîne aura l'indice 0. Cela signifie que vous devez déclarer la position de départ en conséquence lorsque vous utilisez la méthode ```substring()``` ou l'opérateur ```..```. Par exemple, si vous voulez extraire la première lettre d'une chaîne, vous devrez utiliser la position 0 comme ceci : ```str[0]``` ou ```str.substring(0,1)```.

De plus, si vous avez besoin d'extraire la dernière lettre d'une chaîne, vous pouvez utiliser l'indice -1 au lieu d'une position de fin explicite. Par exemple, ```str[str.length - 1]``` renverra la dernière lettre de la chaîne.

Selon vos besoins, vous pouvez également utiliser la méthode ```slice()``` pour extraire plusieurs sous-chaînes à la fois en spécifiant les indices souhaités dans une boucle ou une liste. La méthode ```indexOf()``` peut également être utile pour trouver la position d'un caractère spécifique dans une chaîne, que vous pouvez ensuite utiliser comme paramètre dans la méthode ```substring()```.

## Voir aussi

- [Documentation officielle sur les chaînes de caractères en Kotlin] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Guide pratique pour manipuler les chaînes en Kotlin] (https://www.baeldung.com/kotlin/string-manipulation)