---
title:                "Elixir: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è spesso una parte importante del processo di programmazione in Elixir. Questo permette ai nostri programmi di gestire in modo efficiente l'uso di memoria e di risorse.

## Come Fare

Per creare un file temporaneo in Elixir, possiamo utilizzare la funzione `Tempfile.open/2` della libreria `Tempfile`. Possiamo fornire un prefisso opzionale per il nome del file e specificare un percorso di salvataggio se desideriamo salvarlo in una specifica directory.

```Elixir
iEx> file = Tempfile.open("temp-", "/tmp")
# %Tempfile{path: "/tmp/temp-403517"}
```

Possiamo anche specificare un suffisso opzionale per il nome del file e specificare un estensione se vogliamo creare un file con una specifica estensione.

```Elixir
iEx> file = Tempfile.open("temp-", "/tmp", ".txt")
# %Tempfile{path: "/tmp/temp-467188.txt"}
```

Una volta che abbiamo creato il file temporaneo, possiamo accedervi tramite il percorso specificato nella nostra variabile di file. Possiamo anche leggere e scrivere sul file come se fosse un normale file di testo.

```Elixir
iEx> File.write(file.path, "Questo è un file temporaneo.")
:ok
iEx> File.read(file.path)
"Questo è un file temporaneo."
```

Una volta che abbiamo finito di usare il file temporaneo, possiamo chiuderlo utilizzando il metodo `close/1` sulla nostra variabile di file.

```Elixir
iEx> file.close()
:ok
```

## Approfondimento

Creare un file temporaneo in Elixir è un processo molto utile quando vogliamo gestire in modo efficiente la gestione delle risorse. Possiamo vedere come il percorso del file temporaneo è memorizzato automaticamente dalla libreria `Tempfile` e come possiamo utilizzarlo per accedere al nostro file temporaneo.

## Vedi Anche

- [La documentazione ufficiale della libreria Tempfile](https://hexdocs.pm/tempfile/readme.html)
- [Un buon tutorial su come utilizzare file temporanei in Elixir](https://dev.to/joeverhaeghe/handling-temporary-files-in-elixir-47gd)