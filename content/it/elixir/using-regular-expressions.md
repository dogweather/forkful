---
title:                "L'utilizzo delle espressioni regolari"
html_title:           "Elixir: L'utilizzo delle espressioni regolari"
simple_title:         "L'utilizzo delle espressioni regolari"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Le espressioni regolari in Elixir sono sequenze di caratteri che vengono utilizzate per cercare, sostituire e manipolare testo. Vengono spesso utilizzate dai programmatori per semplificare operazioni di ricerca e manipolazione di stringhe di testo.

## Come fare:

Le espressioni regolari possono essere create utilizzando la libreria Regex di Elixir. Ad esempio, possiamo cercare una parola all'interno di una stringa utilizzando il seguente codice:

```
Elixir Regex.scan("testo di esempio", ~r/testo/)
```

Questo restituirà una lista contenente tutti i match trovati, nel nostro caso "testo".

Per sostituire una porzione di testo con un'altra stringa, possiamo utilizzare la funzione `Regex.replace`:

```
Elixir Regex.replace("testo di esempio", ~r/di/, "del")
```

Questo sostituirà la parola "di" con "del" all'interno della stringa.

## Approfondimento:

Le espressioni regolari sono state inventate negli anni '50 e divennero popolari negli anni '80. Oggi, sono supportate da molti linguaggi di programmazione, compreso Elixir.

In alternativa alle espressioni regolari, alcuni programmatori preferiscono utilizzare le funzioni di manipolazione delle stringhe di Elixir, come `String.replace` e `String.contains?`.

Le espressioni regolari possono sembrare complesse all'inizio, ma una volta che si ha familiarità con la sintassi, possono essere molto potenti nell'elaborazione di dati e testo.

## Vedi anche:

Per ulteriori informazioni sulle espressioni regolari in Elixir, puoi consultare la documentazione ufficiale su [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html).

Inoltre, puoi trovare tutorial e guide su Elixir e le espressioni regolari su siti come [https://www.tutorialspoint.com/elixir/](https://www.tutorialspoint.com/elixir/) e [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html).