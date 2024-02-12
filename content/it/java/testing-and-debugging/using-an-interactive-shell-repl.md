---
title:                "Utilizzo di un interprete interattivo (REPL)"
aliases:
- it/java/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:24.644105-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Un REPL (Read-Eval-Print Loop - Ciclo Leggi-Valuta-Stampa) è una shell interattiva che elabora input singoli degli utenti, esegue codice e restituisce il risultato. I programmatori lo utilizzano per esperimenti rapidi, per il debug o per l’apprendimento, poiché permette un feedback immediato e iterazioni.

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
