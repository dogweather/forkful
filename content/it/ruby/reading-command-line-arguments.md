---
title:                "Lettura degli argomenti dalla riga di comando"
html_title:           "Ruby: Lettura degli argomenti dalla riga di comando"
simple_title:         "Lettura degli argomenti dalla riga di comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se stai iniziando a imparare Ruby, una delle prime cose che probabilmente imparerai è come passare argomenti alla riga di comando. Questo ti permetterà di interagire con il tuo programma in modo più dinamico e personalizzabile.

## Come fare

Per passare argomenti alla riga di comando in Ruby, puoi utilizzare l'array `ARGV`. In questo esempio, stampiamo gli argomenti passati dopo il nome del file:

```Ruby
# Esempio di passare argomenti alla riga di comando

puts "Ciao #{ARGV[0]}!" # Stampa "Ciao" con il primo argomento passato

# In linea di comando digita: `ruby saluti.rb mondo`
# Output: Ciao mondo!
```

Per ottenere tutti gli argomenti passati, puoi utilizzare un ciclo `for` e l'indice `ARGV.length`:

```Ruby
# Esempio di ottenere tutti gli argomenti passati

for i in 0...ARGV.length # Ciclo dall'indice 0 alla lunghezza dell'array ARGV
  puts "Argomento #{i}: #{ARGV[i]}" # Stampa l'argomento corrente con il suo indice
end

# In linea di comando digita: `ruby argomenti.rb ciao mondo !`
# Output: Argomento 0: ciao
# Argomento 1: mondo
# Argomento 2: !
```

## Approfondimento

Oltre ad utilizzare l'array `ARGV`, è possibile utilizzare la gemma `optparse` per gestire in modo più avanzato e strutturato gli argomenti passati alla riga di comando. Questa gemma permette di definire opzioni e switch per il programma e di gestirli tramite un blocco di codice. Ecco un esempio:

```Ruby
# Esempio di utilizzo di optparse

require 'optparse' # Includi la gemma optparse

options = {} # Crea un hash per salvare le opzioni

# Definisci le opzioni e switch da utilizzare
OptionParser.new do |opts|
  opts.banner = "Usage: esempio.rb [opzioni] argomento"

  # Opzione -n, --nome NOME
  opts.on('-n', '--nome NOME', 'Specifica un nome') do |n|
    options[:nome] = n
  end

  # Opzione -a, --anno ANNO
  opts.on('-a', '--anno ANNO', Integer, 'Specifica un anno intero') do |a|
    options[:anno] = a
  end

  # Switch -g, --generoso
  opts.on('-g', '--generoso', 'Imposta il programma in modalità generosa') do
    options[:generoso] = true
  end
end.parse!

# Utilizza le opzioni e switch nel tuo programma
puts "Ciao #{options[:nome]}!" if options[:nome] # Stampa un saluto con il nome specificato
puts "Buon anno #{options[:anno]}!" if options[:anno] # Stampa un augurio con l'anno specificato
 puts "Grazie per essere stato generoso!" if options[:generoso] # Stampa un messaggio per lo switch generoso
```

**Per ulteriori informazioni su come utilizzare la gemma optparse, puoi consultare la documentazione ufficiale e questo tutorial.**

## Vedi anche

- [Documentazione ufficiale di Ruby per gestire gli argomenti della riga di comando](https://docs.ruby-lang.org/en/latest/doc/optparse.html)
- [Tutorial su optparse](https://www.rubyguides.com/2018/08/ruby-optionparser/)