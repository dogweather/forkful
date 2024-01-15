---
title:                "Stampa dell'output di debug"
html_title:           "Elixir: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Il debug è una delle attività più importanti nella scrittura di codice. Senza di essa, sarebbe difficile capire cosa sta accadendo all'interno del nostro programma e individuare eventuali errori. Stampare l'output di debug è un modo efficace per esaminare il funzionamento del nostro codice e risolvere eventuali problemi.

## Come

Per stampare l'output di debug in Elixir, possiamo utilizzare la funzione `IO.inspect/2`. Questa funzione accetta due argomenti: il valore che vogliamo stampare e un elenco di opzioni. In questo esempio, stamperemo il valore `hello`:

```Elixir
IO.inspect("hello")
```

Output:

```
"hello"
```

Possiamo anche passare delle opzioni come secondo argomento per controllare il formato dell'output. Ad esempio, possiamo utilizzare l'opzione `label` per specificare un'etichetta per il nostro output. In questo modo, possiamo distinguere facilmente tra diverse uscite di debug.

```Elixir
IO.inspect("world", label: "Value:")
```

Output:

```
Value: "world"
```

## Deep Dive

Oltre alla semplice stampa di valori, la funzione `IO.inspect/2` può anche essere utilizzata per stampare tutta una struttura di dati come una mappa o una lista. Questo è particolarmente utile quando siamo interessati a esaminare più di un valore contemporaneamente. Inoltre, possiamo utilizzare l'opzione `depth` per specificare la profondità di stampa dei dati complessi, evitando di sovraccaricare l'output con troppe informazioni.

Un altro vantaggio di utilizzare `IO.inspect/2` è che possiamo facilmente disabilitare queste stampe di debug quando non ne abbiamo più bisogno. Infatti, possiamo eliminare rapidamente tutti i `IO.inspect/2` utilizzando la funzione di ricerca e sostituzione del nostro editor di testo.

## Vedi anche

- [Documentazione ufficiale di Elixir sul debug](https://hexdocs.pm/elixir/debugging.html)
- [Articolo su come utilizzare IO.inspect per il debug in Elixir](https://www.lucidchart.com/techblog/2017/09/27/how-and-when-to-use-io-inspect-in-elixir/) 
- [Esempi avanzati di utilizzo di IO.inspect](https://blog.appsignal.com/2019/03/05/using-elixirs-i_o-inspect-in-your-workflow.html)