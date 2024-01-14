---
title:    "Elixir: Verifica se una directory esiste."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché 

Controllare se una directory esiste è un'operazione di base nel processo di programmazione. È importante sapere se una determinata directory esiste prima di eseguire determinate operazioni su di essa, come creare un nuovo file o leggere i file al suo interno. Inoltre, il controllo preventivo della sua esistenza può aiutare a gestire errori e situazioni impreviste durante l'esecuzione del programma.

## Come Fare

In Elixir, possiamo utilizzare la funzione `File.exists?/1` per verificare se una directory esiste o meno. Questa funzione accetta un argomento che rappresenta il percorso della directory che vogliamo controllare. Ad esempio, se vogliamo verificare l'esistenza della directory "/home/documents/", possiamo farlo in questo modo:

```elixir
File.exists?("/home/documents/")
```

Se la directory esiste, la funzione restituirà `true`, altrimenti restituirà `false`.

## Approfondimento

La funzione `File.exists?/1` si basa su una chiamata al sistema operativo per verificare l'esistenza della directory. Se la directory esiste, la chiamata restituirà un risultato positivo, altrimenti verrà generato un errore.

Tuttavia, ci sono alcune situazioni in cui il risultato potrebbe non essere accurato. Ad esempio, se stiamo lavorando con un file system distribuito, potremmo avere una situazione di inconsistenza dei dati in cui la directory esiste solo su alcuni server, ma non su altri.

In questi casi, possiamo utilizzare la funzione `File.stat/1` per ottenere informazioni più dettagliate sulla directory. Questa funzione restituirà un elenco di attributi del file, tra cui uno stato di errore se la directory non esiste. Possiamo quindi utilizzare questa informazione per gestire correttamente l'eccezione o il caso di errore.

## Vedi Anche

- Documentazione ufficiale di Elixir per `File.exists?/1`: https://hexdocs.pm/elixir/File.html#exists?/1
- Documentazione ufficiale di Elixir per `File.stat/1`: https://hexdocs.pm/elixir/File.html#stat/1
- Tutorial Elixir su come gestire errori: https://hexpm.elixir-magic.com/how-to-handle-errors-in-elixir/