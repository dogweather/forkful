---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creazione di File Temporanei in Ruby

## Che Cosa È & Perché?

Creare un file temporaneo significa creare un file che è destinato a essere utilizzato solo per un breve periodo di tempo. Gli sviluppatori lo fanno spesso per gestire i dati di lavoro intermedi senza influenzare i file permanenti.

## Come si Fa:

Di seguito è presente un esempio di come creare ed utilizzare un file temporaneo in Ruby:

```Ruby
require 'tempfile'

temp_file = Tempfile.new('temporary_file')
puts temp_file.path

temp_file.write('Ciao, mondo!')
temp_file.rewind

puts temp_file.read 
temp_file.close
```
L'output fornito sarà il nome del percorso del file temporaneo e il messaggio scritto nel file, cioè 'Ciao, mondo!'.

## Approfondimento

### Contesto Storico

La creazione di file temporanei è una pratica diffusa e presente fin dalle origini dell'informatica. Ruby include la classe Tempfile da versioni molto prime, consentendo una gestione facile e sicura dei file temporanei.

### Alternative

Se si necessita di un file temporaneo che si cancellerà dopo l'uso, Ruby ha un metodo `Tempfile.create`. Questo metodo chiuderà e cancellerà il file dopo il blocco fornito.

### Dettagli di Implementazione

La classe Tempfile di Ruby estende la classe File. Quindi, puoi utilizzare tutte le funzioni disponibili per gli oggetti 'File'. La differenza è che un oggetto Tempfile viene eliminato quando viene garbage collected.

## Vedere Anche

- Documentazione ufficiale di Ruby: https://ruby-doc.org/stdlib/libdoc/tempfile/rdoc/Tempfile.html
- Tempfile su Ruby-Doc.org: http://ruby-doc.org/stdlib/libdoc/tempfile/rdoc/index.html

Ricorda: un buon programmatore è un programmatore pigro. Utilizza i file temporanei per mantenere pulito il tuo sistema di file!