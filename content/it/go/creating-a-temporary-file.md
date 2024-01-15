---
title:                "Creazione di un file temporaneo"
html_title:           "Go: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per creare un file temporaneo in un programma Go. Ad esempio, potresti aver bisogno di salvare temporaneamente dati utilizzati durante l'esecuzione del programma, o di creare un backup di un file prima di modificarlo.

## Come fare

Per creare un file temporaneo in Go, puoi utilizzare la funzione "TempFile" della libreria "io/ioutil". Questa funzione accetta due parametri: il primo è il percorso di un file di base su cui basare il nome del file temporaneo, mentre il secondo è un prefisso da utilizzare per il nome del file. Ad esempio:

```Go
file, err := ioutil.TempFile("/tmp", "go_example")
if err != nil {
    fmt.Println(err)
}
defer file.Close()
```

Questa riga di codice creerà un file temporaneo nella directory "/tmp" con il nome "go_exampleXXXXXXXX". Il suffisso "XXXXXXXX" è un numero generato casualmente per garantire che il nome del file sia univoco.

Puoi anche specificare un'altra directory per creare il file temporaneo, invece di utilizzare la directory predefinita per il sistema operativo. Ad esempio:

```Go
file, err := ioutil.TempFile("/home/user", "go_example")
```

Assicurati di gestire gli errori quando si utilizza questa funzione, poiché potrebbe verificarsi un errore durante la creazione del file temporaneo.

Una volta creato il file temporaneo, puoi utilizzarlo come qualsiasi altro file nel tuo programma Go.

## Approfondimento

Creare un file temporaneo è utile anche per mantenere la pulizia del tuo sistema. I file temporanei vengono creati nella directory temporanea predefinita del sistema operativo, che viene solitamente svuotata periodicamente. In questo modo, non devi preoccuparti di eliminare manualmente i file temporanei creati dal tuo programma.

Inoltre, puoi utilizzare la funzione "TempDir" della libreria "io/ioutil" per creare una directory temporanea anziché un file. Questo è utile se devi elaborare o salvare più file temporanei.

## Vedi anche

- [Documentazione di ioutil.TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Tutorial su come utilizzare i file temporanei in Go](https://www.calhoun.io/creating-random-temp-files-in-go/)
- [Articolo su come utilizzare la funzione TempDir in Go](https://www.ardanlabs.com/blog/2013/11/using-tempdir-to-create-directory.html)