---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:15:25.843954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En REPL (Read-Eval-Print Loop) är ett interaktivt skal som processar enskilda användarinmatningar, exekverar kod och returnerar resultatet. Programmerare använder det för snabba experiment, felsökning eller lärande eftersom det tillåter omedelbar återkoppling och iteration.

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
