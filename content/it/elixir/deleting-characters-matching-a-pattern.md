---
title:                "Eliminazione di caratteri corrispondenti a uno schema"
html_title:           "Elixir: Eliminazione di caratteri corrispondenti a uno schema"
simple_title:         "Eliminazione di caratteri corrispondenti a uno schema"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Eliminare i caratteri corrispondenti a un certo modello è un'operazione comune dei programmatori per trovare e rimuovere delle parti di testo indesiderate da una stringa. Ciò è particolarmente utile quando si lavora con grandi quantità di dati o testo strutturato e si desidera escludere informazioni specifiche.

## Come fare:
In Elixir, è possibile utilizzare la funzione `String.replace/4` per eliminare tutti i caratteri corrispondenti a un certo modello all'interno di una stringa. Di seguito un esempio di come utilizzarla:

```Elixir
stringa = "Otto gatti neri"
risultato = String.replace(stringa, ~r/gatti/, "")
IO.puts risultato
```
Il risultato di questo codice sarà "Otto neri", poiché abbiamo eliminato il termine "gatti" dalla stringa di partenza.

## Approfondimento:
In passato, l'eliminazione di caratteri corrispondenti a un modello veniva solitamente effettuata manualmente tramite l'uso di istruzioni condizionali e cicli. Tuttavia, con l'avvento dei linguaggi di programmazione funzionali come Elixir, è diventato più semplice e veloce utilizzare funzioni di libreria appositamente progettate per svolgere questa operazione. Alcune alternative alla funzione `String.replace/4` sono `Regex.replace/3` e `String.replace_pattern/3`.

Per quanto riguarda l'implementazione, le funzioni di libreria in Elixir utilizzano l'algoritmo di espressioni regolari di Erlang per effettuare la ricerca e la sostituzione dei caratteri all'interno della stringa. Ciò garantisce che l'operazione venga eseguita in modo efficiente e accurato.

## Vedi anche:
- [Documentazione Elixir sulla funzione `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Documentazione Elixir sulla funzione `Regex.replace/3`](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Documentazione Elixir sulla funzione `String.replace_pattern/3`](https://hexdocs.pm/elixir/String.html#replace_pattern/3)