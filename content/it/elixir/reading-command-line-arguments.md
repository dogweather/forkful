---
title:                "Elixir: Lettura degli argomenti dalla riga di comando"
simple_title:         "Lettura degli argomenti dalla riga di comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se stai imparando Elixir o sei un programmatore esperto, è importante avere una buona comprensione di come leggere gli argomenti della riga di comando per poter creare applicazioni efficienti e flessibili. In questo articolo, ti mostrerò come leggere gli argomenti della riga di comando utilizzando Elixir.

## Come

Per leggere gli argomenti della riga di comando, utilizzeremo la funzione `System.argv/0` che restituisce una lista degli argomenti passati al programma. Vediamo un esempio pratico:

```elixir
# Script di esempio
# Nome file: args.exs
# Esegui con: elixir args.exs arg1 arg2 arg3

args = System.argv()

IO.puts("Gli argomenti passati sono: #{inspect args}")
```

Questa prima riga di codice definisce una variabile `args` che contiene la lista degli argomenti passati al programma. Nella successiva riga di codice, stamperemo gli argomenti utilizzando la funzione `IO.puts` e il metodo `inspect` per formattare la lista in un formato più leggibile. Se eseguiamo questo script passando gli argomenti "arg1", "arg2" e "arg3", otterremo questo output:

```elixir
Gli argomenti passati sono: ["arg1", "arg2", "arg3"]
```

Possiamo anche accedere agli argomenti specifici utilizzando l'indice della lista, ad esempio `args[0]` restituirà il primo argomento, in questo caso "arg1". Ora che sappiamo come leggere gli argomenti della riga di comando, possiamo utilizzarli per creare applicazioni più dinamiche e personalizzate.

## Deep Dive

Ci sono diverse opzioni che possiamo utilizzare insieme alla funzione `System.argv/0` per gestire gli argomenti della riga di comando in modo più flessibile. Una di queste opzioni è l'utilizzo della libreria `OptionParser` che ci consente di definire opzioni e argomenti adeguatamente formattati e di generare help e messaggi di errore in modo automatico. Puoi trovare ulteriori informazioni su `OptionParser` nella documentazione di Elixir.

## See Also

Se vuoi approfondire ulteriormente l'argomento degli argomenti della riga di comando in Elixir, ti consiglio di leggere questi articoli:

- [Elixir School - Command Line Applications](https://elixirschool.com/it/lessons/basics/command-line-applications/)
- [Elixir Forum - Command Line Arguments Best Practices](https://elixirforum.com/t/command-line-arguments-best-practices/1205)
- [Elixir Docs - System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)