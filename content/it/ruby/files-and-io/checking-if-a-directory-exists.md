---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:13.361153-07:00
description: "Come fare: La libreria standard di Ruby fornisce metodi semplici per\
  \ controllare l'esistenza di una directory. Ecco come si fa con puro Ruby, senza\u2026"
lastmod: '2024-03-13T22:44:44.065240-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Ruby fornisce metodi semplici per controllare l'esistenza
  di una directory.
title: Verifica se una directory esiste
weight: 20
---

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
