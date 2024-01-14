---
title:                "Elixir: Lettura degli argomenti della riga di comando"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Perché dovresti leggere gli argomenti della riga di comando quando programmi in Elixir? Molte volte, gli argomenti della riga di comando sono utili per passare informazioni dinamicamente al programma e questo può essere molto utile in situazioni come l'automazione di task o l'interazione con l'utente.

## Come procedere

Per leggere gli argomenti della riga di comando in Elixir, dovrai utilizzare il modulo `OptionParser`. Questa libreria ti consente di definire le opzioni che vuoi accettare e di gestire gli argomenti passati in modo efficiente.

Un esempio di codice potrebbe essere il seguente:

```Elixir
defmodule CommandLine do
  def main do
    args = System.argv
    opt_parser = OptionParser.parse(args)

    case opt_parser do
      {opts, _, _} ->
        IO.inspect(opts)
      _ ->
        IO.puts "Errore nella lettura dei parametri"
    end
  end
end
```

L'esempio sopra utilizza il modulo `System` per ottenere gli argomenti della riga di comando, quindi definisce un'istanza del modulo `OptionParser` utilizzando la funzione `parse`. Questa funzione accetta tre parametri: la lista degli argomenti, le opzioni supportate e una lista delle autorizzazioni per specificare i tipi di opzioni.

Se l'esecuzione del codice precedente è:

`elixir command_line.exs --nome utente --formato pdf`

L'output sarà:

`[username: "user", format: "pdf"]`

Per ottenere una migliore gestione degli argomenti, è possibile definire diverse regole utilizzando le opzioni della libreria `OptionParser`. Ad esempio, è possibile impostare un valore di default per un'opzione o specificare un elenco di valori validi per un'opzione.

## Approfondimento

Oltre alla semplice lettura degli argomenti, il modulo `OptionParser` offre anche funzionalità avanzate come la gestione delle opzioni obbligatorie e la gestione degli errori nel caso in cui le opzioni siano passate in un formato non valido.

Inoltre, esistono anche altre librerie che ti consentono di ottenere un maggiore controllo sugli argomenti della riga di comando, come ad esempio `Argparse` o `OptionParser`.

## Vedi anche

- [Documentazione ufficiale del modulo OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Libreria Argparse per Elixir](https://github.com/PragTob/argparse)
- [Libreria OptionParser per Elixir](https://github.com/BlakeWilliams/ElixirOptionParser)