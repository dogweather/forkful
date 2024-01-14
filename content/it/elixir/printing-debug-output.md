---
title:    "Elixir: Stampa dell'output di debug."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché
Debugging è una parte essenziale della programmazione e la stampa di output di debug può aiutare a identificare e risolvere gli errori nel codice. In questo articolo, esploreremo come stampare output di debug in Elixir e perché dovresti farlo.

## Come fare
Stampare output di debug in Elixir è abbastanza semplice. Uno strumento utile è la funzione `IO.inspect/2`, che può stampare qualsiasi valore al suo interno. Di seguito un esempio di come utilizzare questa funzione:

```Elixir
my_var = "Hello"
IO.inspect(my_var)
```

Questo produrrà un output nel terminale come questo:

```Elixir
"Hello"
```

Ciò può essere utile per controllare il valore di una variabile o di una struttura dati durante l'esecuzione del codice.

Un altro metodo per stampare output di debug è utilizzare il modulo `IO.puts/2`, che stampa una stringa di testo sullo standard output. Un esempio di utilizzo è il seguente:

```Elixir
IO.puts("Hello World!")
```

Questo produrrà un output come questo:

```Elixir
Hello World!
```

Se si desidera stampare una stringa di testo con variabili all'interno, è possibile utilizzare la sintassi string interpolation. Ad esempio:

```Elixir
my_var = "Elixir"
IO.puts("Hello #{my_var}!")
```

Questo produrrà un output come questo:

```Elixir
Hello Elixir!
```

Inoltre, è possibile utilizzare il modulo `Kernel.inspect/2` per ottenere una rappresentazione più dettagliata di un valore, inclusi i suoi metadati. Ad esempio:

```Elixir
my_list = [1, 2, 3]
Kernel.inspect(my_list)
```

Questo produrrà un output come questo:

```Elixir
[1, 2, 3]
```

## Deep Dive
Oltre alle funzioni e ai moduli menzionati sopra, ci sono diverse tecniche avanzate che è possibile utilizzare per stampare output di debug in Elixir. Una di queste è l'utilizzo dell'opzione `:label` nella funzione `IO.inspect/2`, che ti consente di etichettare l'output per una migliore comprensione durante la lettura del codice.

Un'altra tecnica è l'utilizzo del modulo `Logger`, che offre funzionalità più avanzate per la stampa di output di debug, come la possibilità di specificare il livello di debug e la possibilità di scrivere l'output su un file di log.

Inoltre, puoi anche usare le direttive di compilazione `@debug` e `@doc` per definire messaggi di debug all'interno del tuo codice e accedere a questi messaggi durante l'esecuzione del programma.

## Vedi anche
Speriamo che questo articolo ti abbia fornito le informazioni necessarie per stampare output di debug in Elixir. Se vuoi saperne di più sul linguaggio e sui suoi strumenti di debug, ecco alcuni link utili:

- [Documentazione ufficiale di Elixir](https://hexdocs.pm/elixir)
- [Guida all'utilizzo della funzione IO.inspect/2](https://elixir-lang.org/getting-started/debugging.html#inspect-io-inspect)
- [Modulo Logger in Elixir](https://hexdocs.pm/logger/Logger.html)
- [Direttive di compilazione in Elixir](https://elixir-lang.org/getting-started/macros.html#directives)