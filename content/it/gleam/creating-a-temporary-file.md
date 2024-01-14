---
title:    "Gleam: Creazione di un file temporaneo"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché 

La creazione di file temporanei è un'operazione utile per molti programmatori Gleam. Può essere utilizzata per salvare dati temporanei durante l'esecuzione del codice o per testare funzionalità senza dover creare file permanenti. In questa guida, impareremo come creare un file temporaneo utilizzando Gleam. 

## Come 

Per creare un file temporaneo in Gleam, dobbiamo prima importare la libreria "FILE" nel nostro codice: 

```Gleam
import file
```

Successivamente, è necessario specificare il percorso e il nome del file temporaneo che vogliamo creare utilizzando la funzione `make_temporary_file()` della libreria "FILE":

```Gleam
let temp_file = file.make_temporary_file("temp_dir/", "my_temp_file.txt")
```

Questo codice creerà un file temporaneo chiamato "my_temp_file.txt" all'interno della directory "temp_dir". 

Una volta creato il file, possiamo scriverci all'interno o utilizzarlo come desideriamo. Per scrivere all'interno del file, possiamo utilizzare la funzione `write()` della libreria "FILE":

```Gleam
file.write(temp_file, "Hello world!")
```

L'output di questo codice sarà "Hello world!" scritto all'interno del file temporaneo. 

## Deep Dive 

Oltre alla funzione `make_temporary_file()`, la libreria "FILE" offre anche altre funzionalità per la gestione dei file temporanei, come la possibilità di specificare manualmente l'estensione del file o di generare una stringa casuale per il nome del file. Inoltre, i file temporanei creati con la libreria "FILE" vengono automaticamente eliminati quando il programma termina o quando vengono chiuse tutte le istanze di file aperte.

## See Also 

- Documentazione ufficiale della libreria "FILE": https://gleam.run/modules/file.html 
- Tutorial su come utilizzare moduli in Gleam: https://gleam.run/book/tutorials/03_modules.html 
- Esempi di utilizzo di file temporanei in Gleam: https://github.com/search?q=language%3AGleam+tempfile&type=Code