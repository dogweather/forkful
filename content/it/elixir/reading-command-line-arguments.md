---
title:                "Lettura degli argomenti da riga di comando"
html_title:           "Elixir: Lettura degli argomenti da riga di comando"
simple_title:         "Lettura degli argomenti da riga di comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando è un modo per far interagire il tuo codice Elixir con l'utente finale. Puoi far sì che il programma riceva input direttamente dall'utente tramite la riga di comando, invece di dover modificare e ricompilare il codice per cambiare i valori delle variabili.

## Come fare:
Elixir ha incluso la libreria [`OptionParser`](https://hexdocs.pm/elixir/OptionParser.html) per semplificare la lettura degli argomenti della riga di comando. Ecco un esempio di come usarlo:
```Elixir
args = OptionParser.parse!(System.argv)
```
Quando esegui questo codice dalla riga di comando, `System.argv` rappresenta tutti gli argomenti passati durante l'esecuzione e `OptionParser.parse!` restituisce una struttura dati contenente i valori degli argomenti come chiavi e valori degli hash. Ad esempio, se esegui questo codice con l'input `elixir program.ex -h true -p 8080`, il risultato sarà:
```Elixir
%{
  "h" => "true",
  "p" => "8080"
}
```

## Approfondimento:
Leggere gli argomenti della riga di comando è un concetto comune nella maggior parte dei linguaggi di programmazione. Tuttavia, in alcuni linguaggi è necessario scrivere del codice aggiuntivo o usare librerie di terze parti per poterlo fare. Inoltre, puoi anche usare [`Application.get_env/2`](https://hexdocs.pm/elixir/Application.html#get_env-2) per ottenere i valori delle variabili di configurazione dalla riga di comando.

## Vedi anche:
- [HexDocs - OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [HexDocs - Application.get_env/2](https://hexdocs.pm/elixir/Application.html#get_env-2)