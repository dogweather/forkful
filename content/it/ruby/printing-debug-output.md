---
title:                "Ruby: Stampa della riga di debug"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante lo sviluppo di un programma in Ruby si possono presentare dei problemi di codice che non è semplice individuare con una semplice lettura. Qui l'utilizzo di output di debug può essere di grande aiuto: stampando a schermo informazioni sullo stato del codice è possibile capire dove si trova l'errore e come risolverlo.

## Come fare

Per stampare output di debug in Ruby si può utilizzare il metodo `puts` seguito da una variabile, una stringa o un'espressione. Ad esempio:

```Ruby
nome = "Mario"
puts "Il nome è #{nome}"  # Stampa: Il nome è Mario
```

In questo caso viene stampata la stringa "Il nome è" seguita dal valore della variabile `nome`. In questo modo si possono stampare a schermo informazioni utili come il valore delle variabili, l'output di una funzione o il risultato di un'operazione.

## Approfondimento

Mentre l'utilizzo di `puts` è un modo molto semplice e immediato per stampare output di debug, esistono anche altri metodi più specifici per specificare il tipo di informazione che si vuole ottenere. Ad esempio, si può utilizzare `p` per stampare una rappresentazione più dettagliata di un oggetto o `print` per stampare senza andare a capo.

Inoltre, per avere un maggior controllo sugli output di debug, si può utilizzare il gem "pry" che permette di mettere in pausa l'esecuzione del codice e accedere a una console interattiva per eseguire comandi e testare il codice.

## Vedi anche

- La documentazione ufficiale di Ruby su `puts`: https://ruby-doc.org/core-3.0.2/IO.html#method-i-puts
- Il gem "pry": https://github.com/pry/pry