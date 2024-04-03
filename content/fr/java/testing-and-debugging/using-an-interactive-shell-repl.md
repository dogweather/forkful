---
date: 2024-01-26 04:15:29.887583-07:00
description: "Un REPL (Read-Eval-Print Loop, boucle de lecture, \xE9valuation et impression)\
  \ est un shell interactif qui traite des entr\xE9es utilisateur uniques, ex\xE9\
  cute du\u2026"
lastmod: '2024-03-13T22:44:57.641733-06:00'
model: gpt-4-0125-preview
summary: "Un REPL (Read-Eval-Print Loop, boucle de lecture, \xE9valuation et impression)\
  \ est un shell interactif qui traite des entr\xE9es utilisateur uniques, ex\xE9\
  cute du code et renvoie le r\xE9sultat."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Comment faire :
Démarrer un REPL en Java est simple avec l'outil `jshell` introduit dans Java 9. Voici comment mettre la main dessus et lancer une session de base :

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  méthode sum(int,int) créée

jshell> sum(5, 7)
$1 ==> 12
```

Quittez à tout moment avec `/exit`.

```Java
jshell> /exit
|  Au revoir
```

## Plongée profonde
Avant `jshell`, les programmeurs Java n'avaient pas de REPL officiel, contrairement aux développeurs Python ou Ruby. Ils utilisaient des IDE ou écrivaient des programmes complets même pour des tâches triviales. `jshell` a été un changement de jeu à partir de Java 9, comblant cette lacune.

Les alternatives incluent les compilateurs en ligne ou les plugins d'IDE, mais ils ne rivalisent pas avec l'immédiateté de `jshell`. En ce qui concerne les mécanismes internes, `jshell` utilise l'API du compilateur Java pour exécuter des fragments de code, ce qui est plutôt soigné. C'est plus qu'un terrain de jeu - il peut importer des bibliothèques, définir des classes, et plus encore. Cela en fait un outil robuste pour le prototypage.

## Voir aussi
- [Guide de l'utilisateur de JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Référence des outils de la plateforme Java, Édition Standard](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [API du compilateur Java](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
