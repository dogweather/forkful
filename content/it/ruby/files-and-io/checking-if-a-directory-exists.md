---
title:                "Verifica se una directory esiste"
aliases: - /it/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:13.361153-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Verificare se una directory esiste in Ruby permette ai programmatori di verificare la presenza di una directory prima di eseguire operazioni come la lettura di file o la creazione di nuove directory. Questo è fondamentale per evitare errori nella gestione dei file e garantire l'affidabilità delle manipolazioni del sistema dei file.

## Come fare:
La libreria standard di Ruby fornisce metodi semplici per controllare l'esistenza di una directory. Ecco come si fa con puro Ruby, senza bisogno di librerie di terze parti:

```ruby
require 'fileutils'

# Verifica se una directory esiste
if Dir.exist?('/percorso/alla/directory')
  puts 'La directory esiste.'
else
  puts 'La directory non esiste.'
end
```
Output di esempio:
```
La directory esiste.
```
Oppure:
```
La directory non esiste.
```

Oltre ad usare `Dir.exist?`, puoi anche utilizzare il metodo `File.directory?` che ritorna `true` se il percorso dato è una directory:

```ruby
if File.directory?('/percorso/alla/directory')
  puts 'La directory esiste.'
else
  puts 'La directory non esiste.'
end
```
Sia `Dir.exist?` che `File.directory?` fanno parte della libreria standard di Ruby e non richiedono gemme esterne per essere usati, rendendoli opzioni comode ed efficienti per i controlli delle directory.
