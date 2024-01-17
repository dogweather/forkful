---
title:                "Leggere un file di testo"
html_title:           "Ruby: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Leggere un file di testo è un'operazione comune per i programmatori. Significa estrarre il contenuto di un file e utilizzarlo all'interno del codice del programma. Ciò consente ai programmatori di gestire dati esterni e integrarli all'interno del loro software.

## Come:
Per leggere un file di testo in Ruby, si può utilizzare il metodo `File.readlines()` che accetta come argomento il percorso del file da leggere. Ad esempio:

```ruby
contents = File.readlines("/path/to/file.txt")

```

Questo riempirà un array, `contents`, con il contenuto del file. Possiamo quindi utilizzare un loop per scorrere il contenuto e farne qualcosa, come stamparlo a schermo:

```ruby
contents.each do |line|
  puts line
end

```

Se vogliamo leggere il contenuto di un file senza riempire l'array, si può utilizzare il metodo `File.foreach()` che restituirà una enumerazione sudo-helper. Ad esempio:

```ruby
File.foreach("/path/to/file.txt") do |line|
  puts line
end

```

## Approfondimento:
La lettura di file di testo è stata una delle prime funzionalità disponibili nei linguaggi di programmazione, essendo essenziale per lavorare con dati esterni. In alternativa al metodo `File.readlines()`, abbiamo anche il metodo `File.read()` che restituisce il contenuto del file come una stringa, utile per file contenenti solo una riga di testo.

Per quanto riguarda l'implementazione, Ruby utilizza il modulo `IO` per gestire l'input/output dei file. Il metodo `File.readlines()` è una combinazione del metodo `IO.readlines()` e del metodo `File.open()`, che apre il file e restituisce il suo contenuto come un array.

## Vedi anche:
- Documentazione ufficiale di Ruby su `File.readlines()`: https://ruby-doc.org/core-3.0.0/IO.html#method-c-readlines
- Documentazione ufficiale di Ruby su `File.foreach()`: https://ruby-doc.org/core-3.0.0/IO.html#method-c-foreach