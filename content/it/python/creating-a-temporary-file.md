---
title:                "Python: Creare un file temporaneo"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Perché

La creazione di file temporanei può essere utile in diverse situazioni di programmazione. Ad esempio, potrebbe essere necessario creare un file temporaneo per salvare alcuni dati e poi cancellarlo alla fine dell'esecuzione del programma.

##Come creare un file temporaneo

Per creare un file temporaneo in Python, possiamo utilizzare il modulo integrato "tempfile". Prima di tutto, importiamo il modulo nel nostro codice:

```Python
import tempfile
```

Per creare un file temporaneo, possiamo utilizzare la funzione "NamedTemporaryFile" del modulo "tempfile". Possiamo specificare il prefisso e il suffisso del nome del file temporaneo, ma questi sono opzionali. Se non vengono forniti, verranno utilizzati valori predefiniti.

```Python
# Creazione di un file temporaneo con nome predefinito
temp_file = tempfile.NamedTemporaryFile()

# Creazione di un file temporaneo con prefisso e suffisso specificati
temp_file2 = tempfile.NamedTemporaryFile(prefix="temp", suffix=".txt")
```

Possiamo scrivere dei dati nel file temporaneo come segue:

```Python
# Scriviamo dei dati nel primo file temporaneo
temp_file.write("Questo è un esempio di file temporaneo")

# Scriviamo dei dati nel secondo file temporaneo
temp_file2.write("Questo è un altro esempio di file temporaneo")
```

Per leggere i dati dal file temporaneo, possiamo utilizzare la funzione "read":

```Python
# Leggiamo i dati dal primo file temporaneo
temp_file.read()

# Leggiamo i dati dal secondo file temporaneo
temp_file2.read()
```

Infine, per cancellare il file temporaneo, possiamo utilizzare il metodo "close":

```Python
# Chiudiamo e cancelliamo il primo file temporaneo
temp_file.close()

# Chiudiamo e cancelliamo il secondo file temporaneo
temp_file2.close()
```

L'output delle operazioni di scrittura e lettura sui due file temporanei sarà il seguente:

```Python
# Output del primo file temporaneo
'Questo è un esempio di file temporaneo'

# Output del secondo file temporaneo
'Questo è un altro esempio di file temporaneo'
```

##Approfondimento

Creare un file temporaneo è utile quando abbiamo bisogno di gestire dei dati temporanei durante l'esecuzione del nostro programma. Tuttavia, è importante notare che i file temporanei vengono automaticamente cancellati quando vengono chiusi, quindi non sono adatti per dati persistenti.

Inoltre, il modulo "tempfile" offre anche altre funzioni utili, come la possibilità di creare una directory temporanea o di ottenere il percorso del file temporaneo creato.

##Vedi anche

- Documentazione ufficiale sul modulo "tempfile": https://docs.python.org/3/library/tempfile.html
- Tutorial su come utilizzare il modulo "tempfile": https://realpython.com/python-tempfile/
- Esempi pratici di creazione e gestione di file temporanei: https://www.geeksforgeeks.org/creating-a-temporary-file-using-tempfile-module-in-python/