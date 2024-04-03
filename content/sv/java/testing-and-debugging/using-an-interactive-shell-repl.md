---
date: 2024-01-26 04:15:25.843954-07:00
description: "Hur man g\xF6r: Att starta en REPL i Java \xE4r enkelt med verktyget\
  \ `jshell` som introducerades i Java 9. S\xE5 h\xE4r f\xE5r du tag p\xE5 det och\
  \ startar en grundl\xE4ggande\u2026"
lastmod: '2024-03-13T22:44:37.788989-06:00'
model: gpt-4-0125-preview
summary: "Att starta en REPL i Java \xE4r enkelt med verktyget `jshell` som introducerades\
  \ i Java 9."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
Att starta en REPL i Java är enkelt med verktyget `jshell` som introducerades i Java 9. Så här får du tag på det och startar en grundläggande session:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  skapade metod sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Avsluta när som helst med `/exit`.

```Java
jshell> /exit
|  Hej då
```

## Fördjupning
Innan `jshell` hade Java-programmerare inte ett officiellt REPL, till skillnad från Python- eller Ruby-utvecklare. De använde IDE:er eller skrev fullständiga program även för triviala uppgifter. `jshell` var en game-changer från och med Java 9, som minskade det gapet.

Alternativ inkluderar onlinekompilatorer eller IDE-plugins, men de matchar inte `jshell`:s omedelbarhet. När det gäller interna funktioner använder `jshell` Java Compiler API för att exekvera kodfragment, vilket är ganska häftigt. Det är mer än en lekplats – det kan importera bibliotek, definiera klasser och mer. Detta gör det till ett robust verktyg för prototypning.

## Se även
- [JShell Användarguide](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition Tools Reference](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
