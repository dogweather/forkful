---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "Elixir: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Elixir, è probabile che tu abbia familiarità con l'uso della riga di comando per eseguire i tuoi script. Ma sai come leggere gli argomenti della riga di comando in Elixir? In questo articolo, imparerai come farlo in modo semplice e intuitivo.

## Come fare

Per leggere gli argomenti della riga di comando in Elixir, è necessario utilizzare il modulo `System` e il suo metodo `argv/0`. Questo metodo restituisce una lista contenente tutti gli argomenti passati al tuo script, escludendo il nome del file.

```Elixir
# Leggi gli argomenti della riga di comando
args = System.argv()

# Stampa la lista degli argomenti
IO.inspect args 
```

Se desideri ottenere solo un argomento specifico, puoi utilizzare l'indice della lista come segue:

```Elixir
# Leggi il primo argomento della riga di comando
arg = System.argv()[0]

# Stampa il primo argomento
IO.inspect arg 

# Output: "Hello"
```

È importante notare che i valori degli argomenti saranno tutti di tipo stringa, quindi se vuoi utilizzarli come numeri o altri tipi di dati, dovrai convertirli manualmente.

## Approfondimento

Il metodo `argv/0` del modulo `System` non è l'unico modo per leggere gli argomenti della riga di comando in Elixir. Puoi anche utilizzare la funzione `Application.get_env/2` per accedere agli argomenti passati da riga di comando tramite la variabile di ambiente `:elixir_cmdline`.

```Elixir
# Leggi gli argomenti della riga di comando
args = Application.get_env(:elixir_cmdline, :argv)

# Stampa la lista degli argomenti
IO.inspect args 
```

Questo metodo è utile se stai sviluppando un'applicazione Elixir che deve leggere gli argomenti della riga di comando durante l'esecuzione.

## Vedi anche

- [Documentazione ufficiale di Elixir sul modulo System](https://hexdocs.pm/elixir/System.html#argv/0)
- [Tutorial su come gestire gli argomenti della riga di comando in Elixir](https://www.poeticoding.com/how-to-handle-command-line-arguments-in-elixir/) 
- [Altro articolo su come leggere gli argomenti della riga di comando in Elixir](https://elixir-lang.org/getting-started/mix-otp/command-line.html#command-line-options-and-environment-variables)