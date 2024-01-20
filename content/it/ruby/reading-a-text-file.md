---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere un file di testo in Ruby consiste nel prendere dei dati da un file testuale e metterli a disposizione per il nostro programma. I programmatori lo fanno per gestire e manipolare dati di grandi volumi, mantenere la persistenza dei dati, o per leggere configurazioni di un sistema apo.

## Come fare:

Per leggere un file di testo in Ruby si utilizza il metodo 'File.open'. Ecco un esempio:

```Ruby
File.open('testo.txt', 'r') do |f|
  while linea = f.gets
    puts linea
  end
end
```

Ciò aprirà 'testo.txt' in modalità di lettura ('r') e stamparà su schermo ogni linea del file. La variabile 'linea' in questo script contiene il contenuto di ogni riga del file di testo durante la lettura.

## Approfondimento:

Nel contesto storico, la necessità di leggere un file di testo risale ai primissimi giorni della programmazione, quando i dati massivi venivano gestiti su nastri magnetici. Ruby, nonostante la sua modernità, conserva questa operazione primitiva per la sua affidabilità e utilità generale.

Come alternativa a 'File.open', Ruby offre anche il metodo 'File.foreach' per leggere un file di testo. Questo metodo è particolarmente utile quando si lavora con file di grande dimensione, poiché legge e restituisce una linea per volta, riducendo così il consumo di memoria.

```Ruby
File.foreach('testo.txt') do |linea|
  puts linea
end
```

Dettagli di implementazione: Ricordatevi sempre di aprire il file in modalità di lettura 'r'. Se avete intenzione di modificare il file, usate 'a' per aggiungere o 'w' per sovrascrivere. Se il file non esiste, 'File.open' restituirà un errore.

## Vedi anche:

Per ulteriori dettagli sulla manipolazione dei file in Ruby, potete fare riferimento alla documentazione ufficiale:[Documentazione Ruby](https://ruby-doc.org/core/File.html). Un'altra risorsa preziosa è il librio "Programming Ruby: The Pragmatic Programmer's Guide", disponibile qui: [Programming Ruby](https://pragprog.com/book/ruby/programming-ruby).