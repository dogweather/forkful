---
title:                "Estrazione di sottosequenze"
html_title:           "Ruby: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
L'estrazione di sottostringhe è un'operazione comune nella programmazione, dove si seleziona una parte di una stringa più grande e la si estrae per usarla in altro codice. Ciò è utile per manipolare e accedere a dati specifici all'interno di una stringa. 

## Come fare:
```Ruby
phrase = "Ciao a tutti!"
 
puts phrase[0,4]
#Output: Ciao

puts phrase[5..9]
#Output: a tu
 
puts phrase[-1]
#Output: !
```

## Approfondimento:
L'estrazione di sottostringhe è stata una funzionalità importante delle prime versioni di Ruby, che rendeva più facile l'accesso ai dati in una stringa. Esistono anche altre opzioni per estrarre sottostringhe in Ruby, come il metodo slice o l'utilizzo di espressioni regolari. Nell'implementazione di Ruby, viene utilizzato l'operatore [] per estrarre una sottostringa, che più comunemente viene utilizzato per accedere agli elementi di un array.

## Vedi anche:
Per saperne di più sulla manipolazione delle stringhe in Ruby, controlla la [documentazione ufficiale](https://ruby-doc.org/core-2.7.0/String.html) di Ruby. Puoi anche esplorare gli [esempi di codice](https://www.rubyguides.com/2018/10/ruby-string-methods/) per comprendere meglio l'utilizzo dell'estrazione di sottostringhe in situazioni diverse.