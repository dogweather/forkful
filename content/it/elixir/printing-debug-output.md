---
title:    "Elixir: Stampa dell'output di debug"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché stampare output di debug

Stampare l'output di debug è un'importante pratica per ogni programmatore Elixir. Ciò consente di verificare il funzionamento del codice e identificare eventuali errori o bug. Inoltre, può aiutare a comprendere meglio il flusso del programma e facilitare la correzione dei problemi. 

## Come procedere

Per stampare l'output di debug in Elixir, si utilizza la funzione `IO.inspect/2`. Questa funzione accetta due argomenti: il primo è il valore da stampare e il secondo è una lista di opzioni per personalizzare l'output. Di seguito un esempio di utilizzo della funzione `IO.inspect/2` all'interno di una funzione:

```Elixir
def my_function(param) do
  # esegue le operazioni necessarie
  IO.inspect(param, label: "Valore di param")
end
``` 

Questa chiamata stampa il valore del parametro `param` insieme alla label personalizzata "Valore di param". In questo modo, sarà più facile identificare quale valore è stato passato alla funzione durante il debug.

## Approfondimento

Oltre all'utilizzo di `IO.inspect/2` per stampare l'output di debug, è possibile anche impostare il flag `debug: :true` sul file di configurazione `config/config.exs`. In questo modo, il sistema automaticamente stampa tutti gli output di debug. Tuttavia, è importante ricordare di disattivare questa opzione in fase di produzione per evitare stampare informazioni sensibili.

Un altro strumento utile per il debug è l'utilizzo del modulo `Logger` di Elixir. Questo modulo fornisce diverse funzioni per la registrazione di messaggi di log a diversi livelli di gravità e diagnostici. Ad esempio, la funzione `Logger.debug/1` può essere utilizzata per stampare un messaggio a scopo di debug senza dover abilitare il flag `debug: :true`. 

## Vedi anche

- Documentazione ufficiale di Elixir su `IO.inspect/2`: [https://hexdocs.pm/elixir/Kernel.html#inspect/2](https://hexdocs.pm/elixir/Kernel.html#inspect/2) 
- Documentazione ufficiale di Elixir su `Logger`: [https://hexdocs.pm/logger/Logger.html](https://hexdocs.pm/logger/Logger.html)