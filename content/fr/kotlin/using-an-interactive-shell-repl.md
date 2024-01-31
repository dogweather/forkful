---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:15:48.350784-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un REPL (Read-Eval-Print Loop, soit Boucle de Lire-Évaluer-Imprimer) est un environnement de programmation informatique simple et interactif. Les programmeurs l'utilisent pour des essais de codage rapides, tester des bouts de code ou apprendre la syntaxe d'un langage sans créer une application complète.

## Comment faire :
Lancer le REPL de Kotlin est un jeu d'enfant. Ouvrez votre terminal et tapez `kotlinc`. Vous arriverez dans le shell Kotlin. Essayons de définir une variable et d'imprimer sa valeur :

```kotlin
Bienvenue dans la version Kotlin 1.7.10 (JRE 1.8.0_292-b10)
Tapez :help pour de l'aide, :quit pour quitter
>>> val greeting = "Bonjour, Kotlin REPL !"
>>> println(greeting)
Bonjour, Kotlin REPL !
```

## Plongée profonde
Le REPL de Kotlin a été lancé avec le langage pour encourager l'expérimentation. Il est similaire au shell interactif de Python, mais adapté à la syntaxe et aux particularités de Kotlin. Des alternatives ? Les environnements interactifs dans les IDEs, comme IntelliJ IDEA, et les terrains de jeu en ligne Kotlin. Le REPL fonctionne en compilant le code à la volée, offrant un retour instantané - crucial pour l'apprentissage et le débogage.

## Voir Aussi
- Documentation Kotlin sur le REPL : [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Essayez Kotlin dans le navigateur : [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Plugin JetBrains Kotlin Playground pour IntelliJ IDEA.
