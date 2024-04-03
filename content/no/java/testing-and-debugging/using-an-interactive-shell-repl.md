---
date: 2024-01-26 04:15:16.492351-07:00
description: "Hvordan: \xC5 starte en REPL i Java er enkelt med `jshell`-verkt\xF8\
  yet introdusert i Java 9. Her er hvordan du kan f\xE5 tak i det og starte en grunnleggende\
  \ \xF8kt."
lastmod: '2024-03-13T22:44:40.668278-06:00'
model: gpt-4-0125-preview
summary: "\xC5 starte en REPL i Java er enkelt med `jshell`-verkt\xF8yet introdusert\
  \ i Java 9."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
Å starte en REPL i Java er enkelt med `jshell`-verktøyet introdusert i Java 9. Her er hvordan du kan få tak i det og starte en grunnleggende økt:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  opprettet metode sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Avslutt når som helst med `/exit`.

```Java
jshell> /exit
|  Ha det
```

## Dypdykk
Før `jshell` hadde ikke Java-programmerere et offisielt REPL, ulikt Python- eller Ruby-utviklere. De brukte IDE-er eller skrev fullstendige programmer selv for trivielle oppgaver. `jshell` var en spillveksler fra og med Java 9, som brobygger den gapet.

Alternativene inkluderer nettbaserte kompilatorer eller IDE-plugins, men de matcher ikke `jshell` sin umiddelbarhet. Når det gjelder interne funksjoner, bruker `jshell` Java Compiler API for å utføre kodefragmenter, noe som er ganske kult. Det er mer enn en lekeplass – det kan importere biblioteker, definere klasser, og mer. Dette gjør det til et robust verktøy for prototyping.

## Se også
- [JShell Brukerguide](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Verktøyreferanse](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
