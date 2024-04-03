---
date: 2024-01-26 04:15:24.644105-07:00
description: "Come fare: Avviare un REPL in Java \xE8 semplice con lo strumento `jshell`\
  \ introdotto in Java 9. Ecco come ottenere e iniziare una sessione di base."
lastmod: '2024-03-13T22:44:43.310315-06:00'
model: gpt-4-0125-preview
summary: "Avviare un REPL in Java \xE8 semplice con lo strumento `jshell` introdotto\
  \ in Java 9."
title: Utilizzo di un interprete interattivo (REPL)
weight: 34
---

## Come fare:
Avviare un REPL in Java è semplice con lo strumento `jshell` introdotto in Java 9. Ecco come ottenere e iniziare una sessione di base:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  creato metodo sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

Uscita in qualsiasi momento con `/exit`.

```Java
jshell> /exit
|  Arrivederci
```

## Approfondimento
Prima di `jshell`, i programmatori Java non avevano un REPL ufficiale, a differenza degli sviluppatori Python o Ruby. Usavano IDE o scrivevano programmi completi anche per compiti banali. `jshell` è stato una svolta a partire da Java 9, colmando quella lacuna.

Le alternative includono compilatori online o plugin per IDE, ma non eguagliano l'immediatezza di `jshell`. Per quanto riguarda i dettagli interni, `jshell` utilizza l'API del compilatore Java per eseguire frammenti di codice, il che è piuttosto interessante. È più di un campo di sperimentazione: può importare librerie, definire classi e molto altro. Questo lo rende uno strumento robusto per il prototipaggio.

## Vedi Anche
- [Guida Utente di JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Riferimento degli Strumenti della Piattaforma Java, Standard Edition](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [API del Compilatore Java](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
