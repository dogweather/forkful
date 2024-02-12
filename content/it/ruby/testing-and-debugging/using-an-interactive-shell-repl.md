---
title:                "Utilizzo di un interprete interattivo (REPL)"
aliases: - /it/ruby/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:19.044288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Una shell interattiva, o REPL (Read-Eval-Print Loop, Ciclo Leggi-Valuta-Stampa), permette di testare il codice in tempo reale. I programmatori la usano per sperimentare, fare debug, e imparare le sfumature di Ruby senza dover creare script completi.

## Come fare:
La REPL di Ruby si chiama IRB (Interactive Ruby). Immergiti e prova Ruby direttamente dal tuo terminale:

```Ruby
irb
2.7.0 :001 > puts "Ciao, mondo di Ruby!"
Ciao, mondo di Ruby!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Approfondimento
Introdotta in Ruby 1.8, IRB è un pilastro per i Rubyisti. Si ispira alle shell interattive di Lisp e Python, fondendo la sperimentazione con un feedback immediato. Alternative come Pry offrono più funzionalità come l'evidenziazione della sintassi e un ambiente di debug più robusto. IRB di per sé è semplice ma può essere potenziata con gemme come 'irbtools' per estenderne la funzionalità. Il modo in cui IRB gestisce il ciclo leggi-valuta-stampa è leggendo ogni riga di input, valutandola come codice Ruby, e poi stampando il risultato, ripetendo questo processo fino all'uscita.

## Vedi Anche
- [IRB di Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [La gemma irbtools](https://github.com/janlelis/irbtools)
