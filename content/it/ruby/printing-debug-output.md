---
title:                "Stampa dell'output di debug"
html_title:           "Bash: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

La stampa del debug output è una pratica che implica la visualizzazione temporanea dei dati di programmazione in uscita per il controllo dei problemi. I programmatori la utilizzano per diagnosticare e risolvere i problemi di codice.

## Come fare:

Ecco un semplice esempio in Ruby:

```Ruby
def divisione(x, y)
  puts "L'input è: x=#{x}, y=#{y}"
  risposta = x / y
  puts "La risposta è: #{risposta}"
rescue ZeroDivisionError
  puts "Errore! Non posso dividere per zero."
end

divisione(10, 2)
divisione(10, 0)
```

In questo caso, il risultato della stampa sarà:

```
L'input è: x=10, y=2
La risposta è: 5
L'input è: x=10, y=0
Errore! Non posso dividere per zero.
```

## Approfondimenti

1. Storicamente, la stampa del debug output esiste dal momento in cui il concetto di programmazione è nato. È uno strumento fondamentale per gli sviluppatori per conoscere meglio il funzionamento del loro codice.

2. Ci sono molte alternative al debug output, come l'utilizzo di un debugger, che fornisce un controllo più dettagliato ma a volte richiede più configurazione.

3. Ruby include una libreria standard `debug` che fornisce funzionalità avanzate di debug. Puoi utilizzarla per esaminare variabili, passare attraverso il codice, mettere in pausa l'esecuzione, ecc.

## Vedi anche

1. Ruby Documentation: Debugging: http://ruby-doc.org/core-2.5.0/Debug.html
2. Debugging Techniques in Ruby (Article): https://www.toptal.com/ruby/debugging-ruby-with-rubys-debug
3. Railscast on Debugging (Video): http://railscasts.com/episodes/54-debugging-ruby-revised