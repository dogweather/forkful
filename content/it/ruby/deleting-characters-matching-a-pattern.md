---
title:                "Ruby: Eliminazione dei caratteri corrispondenti a un modello."
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché cancellare caratteri corrispondenti a un pattern

Cancellare caratteri che corrispondono a un pattern è un'operazione comune nella programmazione Ruby per gestire stringhe e dati. Questo permette di filtrare e manipolare i dati in modo efficiente, rendendo il codice più pulito e leggibile.

## Come fare

Per cancellare caratteri corrispondenti a un pattern in Ruby, è possibile utilizzare il metodo `delete` sulle stringhe. Questo metodo prende come argomento il pattern da cercare e cancellare.

```Ruby
stringa = "Ciao Ruby!"
stringa.delete!("ao")
puts stringa
# output: Ci Ruby!
```

Come si può notare nell'esempio sopra, i caratteri "a" e "o" sono stati eliminati dalla stringa. È importante notare che il metodo `delete!` modifica la stringa originale, mentre il metodo `delete` restituisce una nuova stringa con i caratteri eliminati.

Un'altro modo per cancellare caratteri corrispondenti a un pattern è utilizzare espressioni regolari, definite dalla classe `Regexp`.

```Ruby
stringa = "Ciao Ruby!"
regexp = /ao/
stringa = stringa.gsub(regexp, "")
puts stringa
# output: Ci Ruby!
```

In questo esempio, viene utilizzato il metodo `gsub` per sostituire il pattern con una stringa vuota, ottenendo lo stesso risultato del metodo `delete`.

## Approfondimenti

Cancellare caratteri che corrispondono a un pattern può diventare più complesso quando si iniziano ad utilizzare espressioni regolari avanzate. Inoltre, è importante considerare gli effetti collaterali di modificare stringhe originali o creare nuove stringhe in memoria. Per maggiori informazioni e approfondimenti, consigliamo di consultare la documentazione ufficiale di Ruby sulla classe `String` e sulla gestione di espressioni regolari.

## Vedi anche

- [Documentazione ufficiale di Ruby sulla classe String](https://ruby-doc.org/core-2.7.0/String.html)
- [Documentazione ufficiale di Ruby sulla gestione di espressioni regolari](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Tutorial su espressioni regolari in Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)