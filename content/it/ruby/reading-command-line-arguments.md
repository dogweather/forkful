---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lettura degli argomenti da linea di comando in Ruby

## Che cos'è e perché?
La lettura degli argomenti da linea di comando riguarda il recupero di input dai parametri specificati quando si esegue uno script di Ruby. Gli sviluppatori fanno questo quando vogliono personalizzare l'esecuzione di un programma senza modificare il codice sorgente.

## Come si fa:
Per recuperare gli argomenti da linea di comando in Ruby, usiamo la variabile di sistema predefinita `ARGV`. Ecco un esempio pratico:

```ruby
# mio_script.rb
puts "Hai passato #{ARGV.length} argomenti."
ARGV.each do |arg|
  puts "Argomento: #{arg}"
end
```
Eseguendo lo script con `ruby mio_script.rb arg1 arg2 arg3`, otterremmo:

```
Hai passato 3 argomenti.
Argomento: arg1
Argomento: arg2
Argomento: arg3
```

## Approfondimenti:
La variabile `ARGV` è stata adottata da Ruby dai linguaggi C e Perl. È importante notare che questi argomenti sono sempre interpretati come stringhe. Se hai bisogno di lavorare con tipi di dati diversi, dovrai convertirli.

Esistono vari metodi per leggere gli argomenti da linea di comando oltre `ARGV`, come l'uso di librerie esterne (ad es. `OptionParser`, `Thor`) che offrono una gestione degli argomenti più sofisticata e opzioni di formattazione.

Il funzionamento interno di `ARGV` è piuttosto semplice: è un array di stringhe in cui ogni elemento corrisponde a un argomento da linea di comando. Ruby lo popola automaticamente all'avvio dello script.

## Per saperne di più
Potrebbe essere utile consultare i seguenti link per ulteriori informazioni sugli argomenti da linea di comando in Ruby:

1. [Ruby Doc - ARGV](https://docs.ruby-lang.org/en/latest/variable.html#ARGV)
2. [Ruby Doc - OptionParser](https://docs.ruby-lang.org/en/latest/lib/OptionParser.html)
3. [Stack Overflow - How do I parse command line arguments in Ruby?](https://stackoverflow.com/questions/483210/how-do-i-parse-command-line-arguments-in-ruby)
4. [RubyGuides - Command Line Arguments](https://www.rubyguides.com/2018/09/ruby-argv-command-line-arguments/)