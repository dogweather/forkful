---
title:                "Convertire una stringa in minuscolo"
html_title:           "Ruby: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Converting a string to lower case è un'operazione comune durante lo sviluppo di applicazioni in Ruby. Questo permette di uniformare i dati inseriti dall'utente e semplificare il confronto tra stringhe.

## Come fare

```Ruby
# Esempio 1: Utilizzo di .downcase su una stringa 
stringa = "Ciao, come stai?"
puts stringa.downcase
# output: ciao, come stai?

# Esempio 2: Utilizzo di .downcase! su una stringa 
stringa = "HELLO WORLD"
stringa.downcase!
puts stringa
# output: hello world
```

## Approfondimento

Per convertire una stringa in lower case, Ruby utilizza il metodo `.downcase` o `.downcase!` a seconda se si desidera una nuova stringa convertita o modificare la stringa originale. Entrambi i metodi funzionano nello stesso modo, ma `.downcase!` ha l'effetto collaterale di modificare la stringa originale.
Durante la conversione, il metodo considera anche le lettere accentate e i caratteri speciali come la "ß" tedesca. Inoltre, in Ruby, è possibile utilizzare metodi alternativi come `.casecmp` per confrontare stringhe in modo case insensitive.

## Vedi anche

- [Metodi di stringa in Ruby](https://www.rubyguides.com/ruby-string-methods/)
- [Confronto di stringhe case insensitive in Ruby](https://www.rubydoc.info/docs/ruby/LangFile:upcase=3F)
- [Operazioni comuni con le stringhe](https://victoria.dev/blog/common-string-references-in-ruby/)