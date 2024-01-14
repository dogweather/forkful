---
title:    "Elixir: Creazione di un file temporaneo"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler creare un file temporaneo nel tuo codice Elixir. Potresti avere bisogno di salvare dati temporaneamente durante l'esecuzione della tua applicazione, o forse stai lavorando con librerie esterne che richiedono che i dati vengano scritti su file temporanei.

## Come fare

Per creare un file temporaneo in Elixir, possiamo utilizzare la funzione `File.tmpname/1` che accetta come argomento una stringa facoltativa che funge da prefisso per il nome del file temporaneo. Ad esempio:
```Elixir
file_name = File.tmpname("my_temp_file") 
# Output: "/tmp/my_temp_file23456"

file = File.open!(file_name, [:write]) 
# Apre il file in modalità scrittura 

IO.write(file, "Hello, world!") 
# Scrive sul file

File.close(file)
# Chiude il file
```

## Approfondimento

Se stai creando un file temporaneo per leggere o scrivere dati su una libreria esterna, è importante tenere presente alcune cose. Prima di tutto, assicurati di salvare il percorso del file nel caso in cui sia necessario accedervi più volte durante l'esecuzione del tuo codice. Inoltre, è importante eliminare il file temporaneo dopo averlo utilizzato per evitare di riempire inutilmente lo spazio di archiviazione.

Per eliminare un file temporaneo, puoi utilizzare la funzione `File.rm/1` passando il nome del file come argomento. È una buona pratica aggiungere questa riga di codice alla fine del tuo blocco di codice che utilizza il file temporaneo.

## Vedi anche

Per ulteriori informazioni su come utilizzare file temporanei in Elixir, puoi visitare i seguenti link:

- https://hexdocs.pm/elixir/File.html#tmpname/1
- https://medium.com/@r_rymarz/creating-temporary-files-in-elixir-7091684a047a
- https://elixirschool.com/en/lessons/advanced/io/#tempfiles