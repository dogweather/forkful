---
title:                "Scrivere a standard error"
html_title:           "Ruby: Scrivere a standard error"
simple_title:         "Scrivere a standard error"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Quando scriviamo un programma in Ruby, di solito la nostra prima opzione per visualizzare un output è tramite il metodo `puts`. Tuttavia, c'è un'altra opzione che spesso gli sviluppatori preferiscono: l'uso di `STDERR.puts`. Questa istruzione scrive al cosiddetto "standard error", che è uno dei tre flussi di output disponibili in un programma Ruby.

Ma perché utilizziamo `STDERR.puts` invece di `puts`? Ci sono diverse ragioni per cui gli sviluppatori possono preferirlo. A volte, ci sono dei messaggi di errore che vogliamo che siano mostrati solo in caso di problemi, quindi `STDERR.puts` permette di separare questi messaggi dagli output normali del programma. Inoltre, `STDERR` è sempre disponibile, indipendentemente dalla configurazione di output del programma. Infine, l'uso di `STDERR.puts` è considerato una buona pratica di programmazione, in quanto ci aiuta a mantenere il nostro codice più organizzato e facile da debuggare.

## Come fare:

Per scrivere a standard error in Ruby, è sufficiente utilizzare la seguente sintassi:

```ruby
STDERR.puts "Messaggio di errore"
```

Se vogliamo salvare la nostra istruzione in una variabile, possiamo farlo con `STDERR.puts "Messaggio di errore", var` (usando il secondo parametro opzionale `var`).

Ecco un esempio di output che otterremo utilizzando `STDERR.puts`:

```ruby
STDERR.puts "Errore: file non trovato"
```
Output:
`Errore: file non trovato`

## Approfondimento:

In passato, la scrittura a standard error era molto più comune, ma con l'avvento dei log e dei sistemi di gestione degli errori più sofisticati, non è più così comune. Tuttavia, alcune librerie e framework ancora preferiscono utilizzare `STDERR` per gestire i messaggi di errore.

Un'alternativa a `STDERR.puts` è `STDERR.write`, che accetta una stringa e la scrive direttamente senza aggiungere automaticamente una nuova riga come fa `STDERR.puts`. Questo può essere utile se vogliamo controllare l'output più dettagliatamente.

Per quanto riguarda l'implementazione, è interessante notare che il "standard error" è solo un nome convenzionale e non ha alcuna correlazione con errori reali. Invece, è solo uno dei tre flussi di output standard disponibili in Ruby, gli altri due sono "standard input" e "standard output". 

## Vedi anche:

- [Documentazione ufficiale di Ruby su StandardError](https://ruby-doc.org/docs/keywords/1.9/Object.html#label-StandardError)
- [Spiegazione approfondita di STDOUT e STDERR su TheStreets](https://medium.com/we-are-the-streets/the-difference-between-stderr-and-stdout-cab4175120a7)