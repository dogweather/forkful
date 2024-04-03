---
date: 2024-01-26 04:15:13.145614-07:00
description: "Wie geht das: Ein REPL in Java zu starten ist einfach mit dem `jshell`-Werkzeug,\
  \ das in Java 9 eingef\xFChrt wurde. So bekommen Sie es in die Hand und\u2026"
lastmod: '2024-03-13T22:44:53.764634-06:00'
model: gpt-4-0125-preview
summary: "Ein REPL in Java zu starten ist einfach mit dem `jshell`-Werkzeug, das in\
  \ Java 9 eingef\xFChrt wurde."
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Wie geht das:
Ein REPL in Java zu starten ist einfach mit dem `jshell`-Werkzeug, das in Java 9 eingeführt wurde. So bekommen Sie es in die Hand und starten eine Grundsession:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  Methode sum(int,int) wurde erstellt.

jshell> sum(5, 7)
$1 ==> 12
```

Beenden Sie jederzeit mit `/exit`.

```Java
jshell> /exit
|  Auf Wiedersehen
```

## Tiefer Eintauchen
Vor `jshell` hatten Java-Programmierer keinen offiziellen REPL, im Gegensatz zu Python- oder Ruby-Entwicklern. Sie verwendeten IDEs oder schrieben komplette Programme sogar für triviale Aufgaben. `jshell` war ab Java 9 ein Game-Changer, da es diese Lücke schloss.

Alternativen beinhalten Online-Compiler oder IDE-Plugins, aber sie kommen nicht an die Unmittelbarkeit von `jshell` heran. Was die Interna betrifft, verwendet `jshell` die Java Compiler API, um Codefragmente auszuführen, was ziemlich raffiniert ist. Es ist mehr als ein Spielplatz – es kann Bibliotheken importieren, Klassen definieren und mehr. Dies macht es zu einem robusten Werkzeug für das Prototyping.

## Siehe auch
- [JShell Benutzerhandbuch](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Tools Referenz](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java-Compiler-API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
