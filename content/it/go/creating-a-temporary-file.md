---
title:                "Go: Creazione di un file temporaneo"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è spesso una pratica utile nei programmi Go per creare dati che devono essere utilizzati solo temporaneamente e che non necessitano di essere memorizzati a lungo termine. Ciò può aiutare a risparmiare spazio di archiviazione e mantenere la pulizia dei dati nel tuo sistema.

## Come fare

Per creare un file temporaneo in Go, puoi utilizzare la funzione `ioutil.TempFile()`. Questa funzione accetta due parametri: il primo è la directory in cui desideri creare il file temporaneo e il secondo è un prefisso per il nome del file.

```Go
tempFile, err := ioutil.TempFile("/tmp", "go_temp")
```

Per scrivere dati nel file temporaneo, puoi utilizzare il metodo `WriteString()` del file temporaneo.

```Go
tempFile.WriteString("Questo è un esempio di dati scritti in un file temporaneo.")
```

Infine, per rimuovere il file temporaneo quando non ne hai più bisogno, puoi utilizzare la funzione `Remove()`.

```Go
err = os.Remove(tempFile.Name())
```

## Approfondimento

Se vuoi avere più controllo sulla creazione del file temporaneo, puoi utilizzare la funzione `CreateTemp()` del pacchetto `os`. Questa funzione ti permette di specificare una directory e un prefisso per il nome del file, ma ti dà anche la possibilità di specificare delle opzioni di sicurezza e di impostare manualmente i permessi del file temporaneo.

```Go
tempFile, err := os.CreateTemp("/tmp", "go_temp", os.ModePerm)
```

Inoltre, puoi anche specificare l'estensione del file utilizzando la stringa del suffisso come terzo parametro.

```Go
tempFile, err := os.CreateTemp("/tmp", "go_temp", ".txt")
```

## Vedi anche

- [Documentazione ufficiale di Go su `ioutil.TempFile`](https://golang.org/pkg/io/ioutil/#TempFile)
- [Esempi di utilizzo di `ioutil.TempFile`](https://gobyexample.com/temporary-files)
- [Documentazione ufficiale di Go su `os.CreateTemp`](https://golang.org/pkg/os/#CreateTemp)