---
title:                "Eliminare caratteri corrispondenti a un modello"
html_title:           "Ruby: Eliminare caratteri corrispondenti a un modello"
simple_title:         "Eliminare caratteri corrispondenti a un modello"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

C'è un modo efficiente per eliminare caratteri all'interno di una stringa che corrispondono a un determinato modello di espressione regolare. Questo può essere utile per modificare una stringa o per filtrare i dati in un programma Ruby.

## Come fare

Per eliminare i caratteri che corrispondono a un modello, possiamo utilizzare il metodo `.gsub` su una stringa. Questo metodo prende due argomenti: il modello di espressione regolare che vogliamo cercare e la stringa di sostituzione. Ad esempio, se vogliamo eliminare tutte le vocali da una stringa, possiamo usare il seguente codice:

```Ruby
stringa ="Ciao amici"
stringa.gsub /[aeiou]/, ""  # => "C"
```

Il metodo `.gsub` ricerca tutte le occorrenze del modello di espressione regolare e le sostituisce con la stringa vuota, eliminando così i caratteri corrispondenti.

## Approfondimento

Questo metodo è molto utile per la manipolazione di stringhe e può essere utilizzato in diverse situazioni. Possiamo specificare più modelli di espressione regolare all'interno di una stringa usando il carattere `|` (pipe). Possiamo anche utilizzare un blocco di codice con `.gsub` per eseguire ulteriori elaborazioni sui caratteri corrispondenti.

Un altro approccio è utilizzare il metodo `.delete`, che prende come argomento una stringa di caratteri da eliminare. Ad esempio:

```Ruby
stringa = "Ciao amici"
stringa.delete "aeiou"  # => "C"
```

Entrambi i metodi sono utili per eliminare caratteri corrispondenti a un modello, ma `.gsub` offre maggiori possibilità di manipolazione dei dati. È importante essere consapevoli di ciò che stiamo eliminando e assicurarsi di utilizzare i metodi con cautela.

## Vedi anche

- [Documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/)
- [RegExr - strumento per testare e imparare le espressioni regolari](https://regexr.com/)