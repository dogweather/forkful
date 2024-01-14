---
title:                "Elixir: Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Nella programmazione Elixir, è spesso necessario manipolare le stringhe e i caratteri al loro interno. Una delle operazioni comuni che si possono svolgere è la cancellazione dei caratteri corrispondenti a un determinato modello. In questo articolo vedremo come farlo utilizzando Elixir.

## Come

Per eliminare i caratteri corrispondenti a un modello, utilizzeremo la funzione `String.replace` di Elixir. Questa funzione richiede tre argomenti: la stringa di input, il modello da cercare e il nuovo valore da sostituire al modello.

Ad esempio, se abbiamo una stringa "ciao mondo" e vogliamo eliminare tutte le vocali, possiamo utilizzare il seguente codice:

```Elixir
stringa = "ciao mondo"
String.replace(stringa, ~r/[aeiou]/, "")
```

L'output di questo codice sarà "c mnrd".

Possiamo anche specificare una stringa vuota come terzo argomento per semplicemente eliminare i caratteri corrispondenti al modello senza sostituirli con altro.

```Elixir
stringa = "ciao mondo"
String.replace(stringa, ~r/[aeiou]/, "")
```

L'output di questo codice sarà "c md".

## Deep Dive

La funzione `String.replace` utilizza le espressioni regolari per trovare i caratteri corrispondenti al modello. Le espressioni regolari sono un potente strumento per la manipolazione delle stringhe e la loro sintassi è simile a quella di altri linguaggi di programmazione.

È importante notare che la funzione `String.replace` è case-sensitive, quindi se vogliamo eliminare tutti i caratteri "a" indipendentemente dal loro caso, dobbiamo utilizzare una espressione regolare che include entrambe le opzioni. Ad esempio, `~r/[aA]/` eliminerà sia le "a" minuscole che le "A" maiuscole.

Oltre alla funzione `String.replace`, Elixir offre anche altre funzioni per manipolare le stringhe, come ad esempio `String.trim` per eliminare gli spazi bianchi all'inizio e alla fine di una stringa o `String.split` per dividerla in una lista basandosi su un separatore.

## Vedi anche

- [Documentazione di `String.replace`](https://hexdocs.pm/elixir/String.html#replace/3)
- [Introduzione alle espressioni regolari in Elixir](https://elixirschool.com/it/lessons/basics/pattern-matching/#regular-expressions)