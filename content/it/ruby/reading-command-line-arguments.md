---
title:    "Ruby: Interpretazione degli argomenti della linea di comando"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Scrivere codice in Ruby può sembrare intimidatorio per molti, ma una delle sue caratteristiche più utili è la capacità di leggere gli argomenti della riga di comando. Questo significa che è possibile fornire input ai programmi Ruby direttamente dal terminale, senza dover modificare il codice ogni volta. In questo articolo spiegheremo come leggere gli argomenti della riga di comando in Ruby e come questo può semplificare il processo di sviluppo.

## Come Fare

Per leggere gli argomenti della riga di comando in Ruby, è necessario utilizzare l'oggetto ARGV. Questo contiene tutti gli argomenti passati al programma tramite la riga di comando. Vediamo un esempio:

```Ruby
# Esempio di come leggere gli argomenti dalla riga di comando
nome = ARGV[0]
cognome = ARGV[1]

puts "Ciao #{nome} #{cognome}! Benvenuto nel mondo Ruby!"
```

Se eseguiamo questo codice passando "John" e "Doe" come argomenti, l'output sarà "Ciao John Doe! Benvenuto nel mondo Ruby!".

Ogni elemento nell'oggetto ARGV è una stringa, quindi possiamo utilizzare i metodi delle stringhe per manipolare e accedere ai valori degli argomenti. Ad esempio, se vogliamo ottenere la lunghezza del primo argomento passato, possiamo scrivere `ARGV[0].length`.

## Approfondimento

Oltre a leggere semplici argomenti, possiamo anche gestire opzioni e valori di flag attraverso il modulo OptionParser di Ruby. Questo ci permette di specificare opzioni e valori per il nostro programma nella riga di comando, rendendolo più flessibile e versatile.

Per maggiori informazioni su come utilizzare OptionParser, è possibile consultare la documentazione ufficiale di Ruby qui: https://ruby-doc.org/stdlib-2.6.3/libdoc/optparse/rdoc/OptionParser.html

## Vedi Anche
- https://www.rubyguides.com/2019/06/ruby-command-line-arguments/
- https://www.tutorialspoint.com/ruby/ruby_command_line_arguments.htm
- https://www.geeksforgeeks.org/command-line-arguments-in-ruby-programming/ (in inglese)