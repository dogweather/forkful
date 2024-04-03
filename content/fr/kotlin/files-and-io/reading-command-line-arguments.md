---
date: 2024-01-20 17:56:12.949992-07:00
description: "Lire des arguments de ligne de commande, c'est r\xE9cup\xE9rer des donn\xE9\
  es fournies lorsque vous lancez votre programme. C'est crucial car \xE7a permet\
  \ aux\u2026"
lastmod: '2024-03-13T22:44:57.757034-06:00'
model: gpt-4-1106-preview
summary: "Lire des arguments de ligne de commande, c'est r\xE9cup\xE9rer des donn\xE9\
  es fournies lorsque vous lancez votre programme."
title: Lecture des arguments de ligne de commande
weight: 23
---

## How to:
```kotlin
fun main(args: Array<String>) {
    if(args.isNotEmpty()) {
        println("Voici les arguments de la ligne de commande:")
        args.forEach { arg ->
            println(arg)
        }
    } else {
        println("Aucun argument n'a été fourni.")
    }
}
```

Si on exécute avec `kotlin MonProgramme.kt arg1 arg2`, l'output serait :
```
Voici les arguments de la ligne de commande:
arg1
arg2
```

## Deep Dive
Historiquement, lire les arguments de ligne de commande est aussi vieux que les terminaux eux-mêmes. C'est une façon directe de passer de l'information à un programme, essentielle dans les scripts et les applications de console. Kotlin, comme beaucoup d'autres langages, utilise un tableau de chaînes de caractères `Array<String>` pour capturer ces arguments. On utilise souvent des librairies tierces comme `kotlinx-cli` pour gérer des arguments plus complexes avec des options et des commutateurs.

Il est aussi possible d'analyser les arguments manuellement pour extraire des options plus avancées, mais cela rend le code plus verbeux et sujet à erreurs. Kotlin n'a pas de library standard pour les arguments de ligne de commande contrairement à des langages comme Python, qui a `argparse`.

## See Also
- Kotlin documentation officielle: [https://kotlinlang.org/docs/command-line.html](https://kotlinlang.org/docs/command-line.html)
- Librairie `kotlinx-cli`: [https://github.com/Kotlin/kotlinx-cli](https://github.com/Kotlin/kotlinx-cli)
