---
title:                "Ruby: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Quando scriviamo in Ruby, vogliamo spesso manipolare le stringhe per renderle maggiormente leggibili o formattate in un certo modo. Una delle operazioni più comuni da fare è quella di capitalizzare una stringa. Ciò significa che la prima lettera di ogni parola della stringa sarà resa maiuscola, mentre le altre rimarranno minuscole. In questo blog post, ti mostrerò come fare ciò in modo semplice ed efficace utilizzando il linguaggio di programmazione Ruby.

## Come Fare

Per capitalizzare una stringa, abbiamo bisogno di utilizzare un metodo di Ruby chiamato `capitalize`. Questo metodo prende la stringa su cui vogliamo lavorare e restituisce una nuova stringa con la prima lettera maiuscola. Vediamo un esempio di codice in cui vogliamo capitalizzare il cognome di una persona:

```Ruby
lastname = "rossi"
puts lastname.capitalize
```

L'output di questo codice sarà `Rossi`, poiché il metodo `capitalize` ha reso maiuscola solo la prima lettera della stringa originale. E se volessimo capitalizzare ogni parola della stringa? In questo caso, dobbiamo utilizzare il metodo `titleize`. Vediamo un esempio:

```Ruby
stringa = "io voglio essere capitalizzato!"
puts stringa.titleize
```

L'output sarà `Io Voglio Essere Capitalizzato!`, poiché il metodo `titleize` rende maiuscola la prima lettera di ogni parola della stringa originale.

## Approfondimento

Oltre ai metodi `capitalize` e `titleize`, esistono anche altri metodi utili per manipolare le stringhe in Ruby. Ad esempio, il metodo `upcase` rende maiuscole tutte le lettere della stringa, mentre il metodo `downcase` le rende tutte minuscole. Inoltre, c'è anche il metodo `swapcase` che scambia le lettere maiuscole con quelle minuscole e viceversa. E se volessimo rimuovere i caratteri non alfanumerici da una stringa? Possiamo utilizzare il metodo `gsub` in combinazione con una espressione regolare. Ecco un esempio:

```Ruby
stringa = "Ciao, come stai?"
puts stringa.gsub(/[^0-9a-z ]/i, '')
```

L'output di questo codice sarà `Ciao come stai`, poiché il metodo `gsub` ha rimosso tutti i caratteri non alfanumerici dalla stringa originale. Ci sono molte altre operazioni che possiamo fare con le stringhe in Ruby, quindi ti incoraggio a esplorare ulteriormente la documentazione ufficiale per imparare di più su questo argomento.

## Vedere Anche

- [Documentazione ufficiale Ruby](https://www.ruby-lang.org/it/documentation/)
- [Tutorial su stringhe in Ruby](https://www.rubyguides.com/ruby-tutorial/strings/)