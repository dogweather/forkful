---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:58:14.804324-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Controllare se una directory esiste ci permette di verificare la presenza di una cartella sul filesystem. I programmatori lo fanno per evitare errori durante la lettura/scrittura dei file e per assicurarsi che i percorsi necessari esistano prima di procedere con le operazioni.

## Come fare:
```Ruby
require 'fileutils'

# Controllo se una directory esiste
directory = "/percorso/alla/directory"
if Dir.exist?(directory)
  puts "La directory esiste!"
else
  puts "La directory non esiste. Creiamola ora."
  FileUtils.mkdir_p(directory)
end
```
Output (se la directory esiste):
```
La directory esiste!
```
Output (se la directory non esiste):
```
La directory non esiste. Creiamola ora.
```

## Approfondimento
In passato, i programmatori Ruby potevano utilizzare il metodo `File.exists?`, ma è stato deprecato in favore di `File.exist?` e `Dir.exist?`. Utilizzare `Dir.exist?` è ora la maniera standard per verificare l'esistenza di directory. Se devi gestire il filesystem in modo più avanzato, considera gemme come 'fileutils' per operazioni complesse. I dettagli implementativi di `Dir.exist?` fanno uso di interfacce di basso livello offerte dal sistema operativo, che variano tra UNIX e sistemi Windows—motivo per cui Ruby astrae queste operazioni per offrirci un interfaccia semplice e pulita.

## Vedi Anche
- [FileUtils module](https://ruby-doc.org/stdlib-3.1.0/libdoc/fileutils/rdoc/FileUtils.html)
- [Class: Dir](https://ruby-doc.org/core-3.1.0/Dir.html)
- Ruby API Doc: [File.exist?](https://ruby-doc.org/core-3.1.0/File.html#method-c-exist-3F)
- Guida a [File e I/O in Ruby](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)