---
title:                "Elixir: Stampa dell'output di debug"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug è un'abilità fondamentale per ogni programmatore Elixir. Ci consente di identificare e risolvere rapidamente i bug nei nostri codici, risparmiando tempo e fatica.

## Come fare

Per stampare output di debug in Elixir, possiamo utilizzare la funzione `IO.inspect/2`. Questa funzione prende due argomenti: il primo è il valore che vogliamo stampare, mentre il secondo è un elenco opzionale di opzioni che controllano il formato e la visualizzazione dell'output.

Ecco un semplice esempio di codice che utilizza `IO.inspect/2`:

```Elixir
name = "Maria"
IO.inspect(name, label: "Nome")
```

Questo codice stampa l'output `Nome: "Maria"` sulla console. Possiamo anche utilizzare l'opzione `:inspect` per specificare come vogliamo che il valore venga visualizzato. Ad esempio:

```Elixir
age = 30
IO.inspect(age, label: "Età", inspect: :binary)
```

Questo codice stamperà `Età: "11110₂"`, dove `11110₂` è la rappresentazione binaria di 30.

## Approfondimento

Oltre alla semplice stampa di output, `IO.inspect/2` ci offre una serie di opzioni avanzate per aiutarci a esaminare i nostri valori. Possiamo utilizzare l'opzione `:depth` per specificare a quanti livelli dobbiamo andare a fondo nell'ispezione di una struttura dati complessa, ad esempio una mappa o una lista nidificata.

Oltre a ciò, `IO.inspect/2` può essere utilizzato in combinazione con l'operatore pipe `|>` per esaminare i valori di una pipeline. Ad esempio:

```Elixir
[1, 2, 3] |> Enum.map(&(&1 * 2)) |> IO.inspect(label: "Molti per 2")
```

Questo codice stamperà l'output `Molti per 2: [2, 4, 6]`, mostrando il risultato della mappatura della lista iniziale.

## Vedi anche

- [Documentazione Elixir su IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Articolo Medium sull'utilizzo di IO.inspect per il debug di Elixir](https://medium.com/elixir-notes/elixir-how-to-debug-with-io-inspect-683a1295c84e)