---
title:    "Elixir: Leggere gli argomenti dalla riga di comando"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler leggere gli argomenti dalla riga di comando in Elixir. Potresti essere un nuovo programmatore che vuole imparare un nuovo linguaggio, o fare parte di un team che sta migrando da un altro linguaggio, o forse hai una specifica necessità di lettura dei parametri per il tuo progetto. In ogni caso, conoscere come leggere gli argomenti dalla riga di comando in Elixir è un'abilità molto utile da avere.

## Come fare

Per leggere gli argomenti dalla riga di comando, è necessario utilizzare la funzione `System.argv()` che restituisce una lista contenente tutti gli argomenti passati. Ad esempio, se eseguiamo il seguente codice in una riga di comando:

```Elixir 
args = System.argv()
IO.inspect(args)
```

L'output sarà una lista contenente tutti gli argomenti della riga di comando.

```
[in: "input.txt", out: "output.txt", config: "config.json"]
```

Si noti che il primo elemento della lista sarà sempre il nome del file eseguibile Elixir, quindi i nostri argomenti iniziano dall'indice 1.

Per accedere a un argomento specifico, possiamo utilizzare la notazione con parentesi quadrate e specificare l'indice dell'elemento desiderato. Ad esempio, se vogliamo accedere al secondo argomento (output.txt), possiamo scrivere `args[2]`.

## Approfondimento

Oltre alla funzione `System.argv()`, esistono anche altri modi per leggere gli argomenti dalla riga di comando in Elixir. Ad esempio, è possibile utilizzare il modulo `OptionParser` che permette di definire opzioni e argomenti specifici per il programma e di gestirli in modo più strutturato.

Inoltre, è importante tenere presente che gli argomenti della riga di comando in Elixir sono passati come stringhe. Quindi, se hai bisogno di convertirli in un altro tipo di dato, come un intero o un booleano, sarà necessario utilizzare funzioni di conversione come `String.to_integer()` o `String.to_boolean()`.

## Vedi anche

- Documentazione ufficiale su `System.argv()`: https://hexdocs.pm/elixir/System.html#argv/0
- Tutorial su `OptionParser`: https://elixirschool.com/en/lessons/advanced/command-line-args/
- Conversione di stringhe in Elixir: https://hexdocs.pm/elixir/String.html#to_integer/1