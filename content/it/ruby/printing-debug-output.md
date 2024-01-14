---
title:                "Ruby: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stai scrivendo un codice Ruby e incontri un bug o un errore che non riesci a risolvere? La stampa di output di debug potrebbe essere la soluzione che stai cercando! Con qualche linea di codice strategicamente posizionata, puoi facilmente visualizzare variabili, valori e informazioni utili durante l'esecuzione del tuo programma.

## Come

### Utilizzare il metodo "puts"

Il metodo "puts" è un modo semplice e veloce per stampare un output di debug. Basta aggiungere la variabile o il valore che vuoi visualizzare all'interno del metodo. Ad esempio:

```Ruby 
puts "La variabile x contiene il valore #{x}"
```

Questa linea di codice stamperà un messaggio con il valore attuale della variabile "x" durante l'esecuzione del programma.

### Utilizzare il metodo "p"

Se vuoi una stampa più dettagliata, puoi utilizzare il metodo "p". Questo metodo stamperà il valore della variabile insieme al suo nome e al suo tipo di dato. Ad esempio:

```Ruby
p x
```

Questa linea di codice stamperà qualcosa del tipo "x = 10 (Integer)".

## Deep Dive

Oltre ai metodi "puts" e "p", esistono anche altri modi per stampare un output di debug. Ad esempio, puoi utilizzare il metodo "pp" per stampare una struttura dati in modo più leggibile o il metodo "inspect" per visualizzare tutti gli attributi di un oggetto.

Inoltre, puoi anche utilizzare la gemma "pry" per eseguire il debugging del tuo codice in modo più interattivo. Questa gemma consente di mettere il programma in pausa e di esplorare variabili e oggetti in un ambiente più dinamico.

## Vedi anche

- https://www.tutorialspoint.com/ruby/ruby_debugger.htm
- https://youtu.be/Pgvsje4Tpzc
- https://davidvgalbraith.com/ruby/2014/guide-debbuging-ruby/
- https://github.com/pry/pry