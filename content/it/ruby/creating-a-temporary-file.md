---
title:                "Ruby: Creare un file temporaneo"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è una pratica comune nella programmazione Ruby che può essere utile in molte situazioni diverse. Un file temporaneo è un file che viene creato e utilizzato temporaneamente, poi eliminato una volta che non è più necessario. Potrebbe essere utile quando si lavora con dati temporanei o si vuole evitare di riempire in modo permanente la memoria del computer con file non necessari.

## Come fare

Per creare un file temporaneo in Ruby, si può utilizzare il metodo `Tempfile.new`. Il seguente codice mostra come creare un file temporaneo e scriverci del contenuto:

```Ruby 
require 'tempfile'

tempfile = Tempfile.new('my_temp_file')
tempfile.write('Questo è il mio contenuto!')
tempfile.rewind
puts tempfile.read

# Output: Questo è il mio contenuto!
```

Una volta terminato l'utilizzo del file temporaneo, è importante eliminare il file con il metodo `tempfile.delete`.

## Approfondimento

Quando si utilizza `Tempfile.new`, viene creato un file con un nome casuale nella directory di sistema temporanea. Inoltre, viene creato in modalità di lettura/scrittura con la possibilità di scrivere o leggere dal file. In questo modo, si può utilizzare il file temporaneo come un file normale all'interno del proprio codice.

Per impostare una directory diversa da quella di sistema temporanea per il file temporaneo, si può utilizzare il parametro `dir` quando si chiama `Tempfile.new`. Inoltre, si può specificare un prefisso per il nome del file con il parametro `prefix`.

## Vedi anche

- [Documentazione di `Tempfile`](https://ruby-doc.org/stdlib-2.7.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby: Come creare un file temporaneo](https://www.rubyguides.com/2015/06/ruby-tempfile/)
- [Esempi pratici di utilizzo di `Tempfile`](https://www.rubyguides.com/2018/09/ruby-tempfile-examples/)