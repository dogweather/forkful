---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:06.833963-07:00
description: "Scrivere su un file di testo in Ruby \xE8 un'operazione fondamentale\
  \ che consente di conservare output e dati in modo persistente, rendendo possibili\u2026"
lastmod: '2024-03-13T22:44:44.069224-06:00'
model: gpt-4-0125-preview
summary: "Scrivere su un file di testo in Ruby \xE8 un'operazione fondamentale che\
  \ consente di conservare output e dati in modo persistente, rendendo possibili l'accesso\
  \ o la modifica dei dati in seguito."
title: Scrivere un file di testo
weight: 24
---

## Come fare:
Ruby rende le operazioni sui file semplici. Per scrivere su un file, puoi utilizzare la classe incorporata `File` di Ruby. L'esempio seguente dimostra come aprire un file per la scrittura (modalità `"w"`) e per l'append (modalità `"a"`), poi scrivere una stringa al suo interno e assicurarsi che il file venga chiuso in seguito:

```ruby
# Scrivere nuovo contenuto su un file, sovrascrivendo il contenuto esistente
File.open("example.txt", "w") do |file|
  file.puts "Ciao, Ruby!"
end

# Appendere contenuto alla fine di un file
File.open("example.txt", "a") do |file|
  file.puts "Aggiungendo un'altra riga."
end
```
Dopo aver eseguito entrambi i frammenti, il contenuto di `example.txt` sarà:
```
Ciao, Ruby!
Aggiungendo un'altra riga.
```

### Utilizzo di una libreria di terze parti: FileUtils
Per operazioni sui file più complesse, può tornare utile la libreria standard di Ruby `FileUtils`, anche se per la scrittura di file base, i metodi standard della classe `File` sono sufficienti. Tuttavia, se vuoi copiare, spostare, rimuovere o eseguire altre operazioni sul filesystem in congiunzione con la scrittura di file, `FileUtils` è da esplorare.

Un esempio di utilizzo di `FileUtils` per creare una directory e poi scrivere un file all'interno di quella directory:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/oggi.log", "w") do |file|
  file.puts "Voce di registro: #{Time.now}"
end
```

Questo dimostra la creazione di una nuova directory `logs` se questa non esiste già, e la scrittura in un nuovo file `oggi.log` al suo interno, mostrando sia la manipolazione delle directory che dei file senza scrivere direttamente con FileUtils, ma sfruttando la sua capacità di gestione delle directory.
