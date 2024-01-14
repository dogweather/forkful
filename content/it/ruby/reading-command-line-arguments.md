---
title:    "Ruby: Lettura degli argomenti della riga di comando"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando in Ruby

Leggere gli argomenti della riga di comando è un'abilità fondamentale per ogni programmatore Ruby. Questa pratica consente di creare programmi interattivi e dinamici, in grado di ricevere input dall'utente.

## Come leggere gli argomenti della riga di comando in Ruby

In Ruby, gli argomenti della riga di comando sono visualizzati come un array denominato `ARGV`. Per accedere a questo array, è sufficiente utilizzare il metodo `ARGV`. Ecco un esempio di codice che stampa tutti gli argomenti inseriti nella riga di comando:

```Ruby
puts "Gli argomenti della riga di comando sono: #{ARGV}"
```

Se eseguiamo il programma con `ruby nome_programma.rb arg1 arg2`, l'output sarà:

```
Gli argomenti della riga di comando sono: ["arg1", "arg2"]
```

Possiamo anche accedere a un singolo argomento utilizzando l'indice desiderato come per un normale array. Ad esempio, se vogliamo stampare solo il secondo argomento, possiamo utilizzare `ARGV[1]`:

```Ruby
puts "L'argomento inserito è: #{ARGV[1]}"
```

L'output sarà quindi:

```
L'argomento inserito è: arg2
```

## Approfondimento su come leggere gli argomenti della riga di comando

Oltre all'utilizzo del metodo `ARGV`, è possibile utilizzare la gemma `optparse` per facilitare la lettura e l'interpretazione degli argomenti della riga di comando. Questa gemma permette di creare opzioni e argomenti con una sintassi intuitiva. Ad esempio, possiamo creare un'opzione per stampare una stringa personalizzata come output:

```Ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.banner = "Utilizzo: esempio.rb [opzioni]"

  opts.on("-s", "--string STRING",
          "Stampa una stringa personalizzata") do |s|
    options[:string] = s
  end
end.parse!

puts "La stringa inserita è: #{options[:string]}" if options[:string]
```

Eseguendo il programma con `ruby nome_programma.rb -s "Ciao mondo!"`, l'output sarà:

```
La stringa inserita è: Ciao mondo!
```

## Vedi anche

- [Documentazione ufficiale di Ruby sugli argomenti della riga di comando](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Documentazione della gemma OptParse](https://github.com/ruby/optparse)