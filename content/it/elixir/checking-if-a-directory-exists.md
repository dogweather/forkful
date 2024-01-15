---
title:                "Verifica se una directory esiste."
html_title:           "Elixir: Verifica se una directory esiste."
simple_title:         "Verifica se una directory esiste."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare l'esistenza di una directory può essere utile in diverse situazioni di programmazione, ad esempio per verificare se un determinato file è presente o per garantire che una directory sia già stata creata prima di eseguire un'operazione su di essa. Inoltre, questa è una buona pratica per evitare errori e migliorare la gestione dei file nel tuo codice.

## Come fare

Per controllare se una directory esiste in Elixir, puoi utilizzare la funzione `File.exists?/1` passando il path della directory come argomento. Vediamo un esempio:

```Elixir
File.exists?("path/alla/directory")
```

Output:

```
true
```

Se la directory esiste, la funzione restituirà `true`, altrimenti restituirà `false`.

Se vuoi controllare l'esistenza di una directory relativa al tuo progetto Elixir, puoi utilizzare la macro `__DIR__` che restituisce il path assoluto della directory del file in cui è chiamata. In questo modo, puoi evitare di specificare il path completo e rendere il tuo codice più portabile.

```Elixir
File.exists?(__DIR__ <> "/path/relativa")
```

Output:

```
true
```

## Ricerca approfondita

Anche se controllare l'esistenza di una directory sembra un'operazione semplice, è importante considerare alcune sfumature. Innanzitutto, la funzione `File.exists?/1` restituirà sempre `false` se il path specificato è un file invece di una directory. Quindi, se il tuo obiettivo è verificare l'esistenza di un file, dovrai utilizzare la funzione `File.regular?/1`.

Inoltre, è importante notare che la funzione `File.exists?/1` restituirà `false` anche se la directory esiste, ma non è leggibile dal processo che esegue il tuo codice. Per esempio, se la directory ha i permessi impostati su "sola lettura" per l'utente corrente, la funzione restituirà `false`. In questo caso, dovrai utilizzare la funzione `File.readable?/1` per verificare se hai i permessi necessari per leggere la directory.

## Vedi anche

- La documentazione ufficiale di Elixir sulla funzione `File.exists?/1`: https://hexdocs.pm/elixir/File.html#exists?/1
- La guida ufficiale di Elixir sulla gestione dei file: https://elixir-lang.org/getting-started/file.html