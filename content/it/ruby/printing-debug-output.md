---
title:                "Stampa dell'output di debug"
date:                  2024-01-20T17:53:29.333388-07:00
model:                 gpt-4-1106-preview
simple_title:         "Stampa dell'output di debug"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Stampare output per debug è mostrare informazioni interne di un programma durante la sua esecuzione. Programmatori lo fanno per capire meglio come il codice si comporta in vivo, per scovare bug o problemi di prestazioni.

## How to (Come fare)
Ruby rende super semplice stampare cose a schermo. Ecco un esempio:

```Ruby
puts "Questa è una stampa di debug"

def aggiungi_e_stampa(a, b)
  risultato = a + b
  p "Aggiungendo #{a} e #{b}, otteniamo #{risultato}"
  risultato
end

aggiungi_e_stampa(7, 3)
```

Output:
```
Questa è una stampa di debug
"Aggiungendo 7 e 3, otteniamo 10"
```

Ci sono anche altri metodi, come `print` e `p`, ciascuno con lievi differenze. Per esempio `p` mostra una versione più "raw" dell'oggetto:

```Ruby
print "print non aggiunge una nuova riga automaticamente."
puts "puts aggiunge una nuova riga alla fine."
p { chiave: "valore" }
```

Output:
```
print non aggiunge una nuova riga automaticamenteputs aggiunge una nuova riga alla fine.
{:chiave=>"valore"}
```

## Deep Dive (Approfondimento)
La stampa per debug ha origini che si perdono nella notte dei tempi informatici, usata fin dalle prime macchine programmabili per capire cosa cavolo stessero facendo. In Ruby possiamo usare `puts`, `print`, e `p` per scopi simili ma con tocchi diversi. `puts` è ottimo per messaggi umani leggibili, `print` quando non vuoi automaticamente una nuova riga, e `p` per vedere la rappresentazione esatta di un oggetto Ruby.

Esistono alternative più sofisticate, come l'uso di un sistema di logging con livelli di severità o l'impiego di gemme come Pry per un'interazione più viva e ricca con il codice. In ambiente di produzione, i log sono imprescindibili, e spesso vanno strutturati per essere parsati da strumenti di analisi.

Un altro aspetto da considerare è il performance impact: troppe stampe possono rallentare l'esecuzione e intasare log files. Usa quindi il debug output con giudizio, e ricorda di pulire quando hai finito.

## See Also (Vedi Anche)
- [Ruby Docs on Kernel#puts](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-puts)
- [Ruby Docs on Kernel#print](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-print)
- [Ruby Docs on Kernel#p](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p)
- [Better Errors gem, per un debug più dettagliato](https://rubygems.org/gems/better_errors/)
- [Pry: a powerful alternative to the standard IRB shell for Ruby](https://pry.github.io/)

Ricorda: il debug è parte integrante del ciclo di vita del software. Impara ad amarlo!