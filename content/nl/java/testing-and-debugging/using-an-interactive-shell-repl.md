---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:28.207931-07:00
description: "Een REPL (Read-Eval-Print Loop) is een interactieve shell die enkele\
  \ gebruikersinvoeren verwerkt, code uitvoert en het resultaat teruggeeft. Programmeurs\u2026"
lastmod: '2024-03-11T00:14:24.504337-06:00'
model: gpt-4-0125-preview
summary: "Een REPL (Read-Eval-Print Loop) is een interactieve shell die enkele gebruikersinvoeren\
  \ verwerkt, code uitvoert en het resultaat teruggeeft. Programmeurs\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
Een REPL (Read-Eval-Print Loop) is een interactieve shell die enkele gebruikersinvoeren verwerkt, code uitvoert en het resultaat teruggeeft. Programmeurs gebruiken het voor snelle experimenten, debugging of leren, omdat het onmiddellijke feedback en iteratie mogelijk maakt.

## Hoe te:
Een REPL in Java starten is eenvoudig met de `jshell`-tool, geïntroduceerd in Java 9. Hier is hoe je ermee aan de slag kunt en een basis sessie kunt starten:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  methode sum(int, int) gecreëerd

jshell> sum(5, 7)
$1 ==> 12
```

Verlaat op elk moment met `/exit`.

```Java
jshell> /exit
|  Tot ziens
```

## Diepgaand
Voor `jshell` hadden Java-programmeurs geen officiële REPL, in tegenstelling tot Python of Ruby-ontwikkelaars. Ze gebruikten IDE's of schreven volledige programma's zelfs voor triviale taken. `jshell` was een game-changer vanaf Java 9, die die kloof overbrugde.

Alternatieven zijn online compilers of IDE-plugins, maar die kunnen niet tippen aan de onmiddellijkheid van `jshell`. Wat de interne werking betreft, gebruikt `jshell` de Java Compiler API om codefragmenten uit te voeren, wat behoorlijk netjes is. Het is meer dan een speeltuin - het kan bibliotheken importeren, klassen definiëren, en meer. Dit maakt het tot een robuust hulpmiddel voor prototyping.

## Zie ook
- [JShell Gebruikersgids](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Tools Referentie](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
