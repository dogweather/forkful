---
title:                "Utilizzo di un interprete interattivo (REPL)"
date:                  2024-01-26T04:14:31.435695-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Un REPL, acronimo di Read-Eval-Print Loop (Ciclo Leggi-Valuta-Stampa), è uno strumento di programmazione per eseguire codice interattivamente e vedere i risultati all'istante. I programmatori lo utilizzano per sperimentare, debuggare o imparare un nuovo linguaggio al volo come Gleam.

## Come fare:

Attualmente Gleam non include un REPL nella sua distribuzione standard. Tuttavia, puoi sperimentare con il codice Gleam utilizzando la shell Erlang esistente perché Gleam compila in bytecode Erlang. Ecco come:

1. Compila il tuo codice Gleam in Erlang.
```plaintext
gleam build
```

2. Avvia la shell Erlang.
```plaintext
erl -pa ebin
```

3. Chiama le tue funzioni Gleam (assumendo che tu abbia un modulo denominato `my_mod` e una funzione `my_fun`).
```erlang
my_mod:my_fun().
```

Dovresti vedere l'output della tua funzione visualizzato nella shell.

## Approfondimento

Il REPL incarna lo spirito dinamico ed esplorativo di molte lingue di programmazione funzionale, risalendo al REPL di LISP negli anni '60. Comparativamente, altri sistemi come `ipython` di Python o `irb` di Ruby offrono esperienze simili per le loro comunità.

Sebbene Gleam non abbia ancora un REPL nativo, sfruttare la shell Erlang rimane un'astuzia di ripiego. Le capacità della shell Erlang derivano dalla BEAM VM, la macchina virtuale che alimenta l'ecosistema Erlang, che include Elixir, LFE e Gleam.

Le alternative ai REPL nell'ecosistema Gleam potrebbero includere la scrittura di casi di test o l'utilizzo di compilatori online e playground di codice che supportano Gleam, per testare frammenti di codice al di fuori di una configurazione di progetto completa.

L'implementazione di un REPL dedicato a Gleam si scontra principalmente con la natura compilata di Gleam e il runtime di Erlang, dove lo scambio di codice a caldo è la norma. Qualsiasi futuro REPL di Gleam avrebbe bisogno di conciliare la tipizzazione statica del linguaggio con l'ambiente di esecuzione dinamico che un REPL si aspetta.

## Vedi Anche

- Documentazione ufficiale di Gleam: https://gleam.run/book/
- Documentazione della shell Erlang: http://erlang.org/doc/man/erl.html
- Un playground del compilatore Gleam online: https://gleam.run/compiler/
