---
title:    "Ruby: Leggere un file di testo"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
La lettura dei file di testo è una delle attività più comuni per i programmatori. Comprendere come farlo correttamente è essenziale per l'elaborazione di dati e la creazione di programmi affidabili.

## Come Fare
Per leggere un file di testo in Ruby, è necessario utilizzare il metodo `File.read()`. Questo metodo accetta come argomento il percorso del file da leggere e restituisce una stringa con il contenuto del file. Ecco un esempio di codice:

```Ruby
content = File.read("file.txt")
puts content
```

Il codice sopra leggerà il contenuto del file `file.txt` e lo stamperà a schermo. Se il file non si trova nella stessa cartella del tuo programma, assicurati di specificare il percorso completo del file.

## Un Approfondimento
La lettura dei file di testo può essere un po' più complessa quando si vogliono gestire file di grandi dimensioni. In questo caso, è consigliabile utilizzare il metodo `File.foreach()`. Questo metodo legge il file linea per linea senza caricare tutto il contenuto in memoria. Ecco un esempio:

```Ruby
File.foreach("file.txt") do |line|
  puts line
end
```

In questo esempio, ogni riga del file viene stampata a schermo grazie all'utilizzo del metodo `puts`. Ciò rende il processo più efficiente in caso di file molto grandi.

## Vedi Anche
- [Documentazione ufficiale di Ruby su File](https://ruby-doc.org/core-2.6/File.html)
- [Tutorial su come leggere i file in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)