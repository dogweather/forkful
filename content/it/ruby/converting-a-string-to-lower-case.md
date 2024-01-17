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

## Cosa & Perché?

Convertire una stringa in minuscolo è un'operazione comune nella programmazione che consiste nel rendere tutti i caratteri di una stringa in minuscolo. I programmatori spesso lo fanno per rendere più facile la comparazione di stringhe o per uniformare l'input dell'utente.

## Come fare:

```Ruby
stringa = "Ciao a Tutti"

puts stringa.downcase
# output: ciao a tutti

puts stringa
# output: Ciao a Tutti
```

## Approfondimento:

Convertire le stringhe in minuscolo è una pratica comune nella programmazione moderna, ma ha le sue origini negli albori dei computer. Prima dell'avvento dei moderni linguaggi di programmazione, le stringhe venivano memorizzate in maiuscolo nei computer, in quanto era più efficiente in termini di spazio di memoria. Oggi esistono anche altre opzioni per manipolare le stringhe, come ad esempio utilizzare metodi che consentono di confrontare le stringhe senza doverle prima convertire in minuscolo.

## Vedi anche:

Per ulteriori informazioni su metodi di manipolazione delle stringhe in Ruby, puoi consultare la documentazione ufficiale: https://ruby-doc.org/core-2.7.2/String.html