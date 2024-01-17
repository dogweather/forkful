---
title:                "Cancellare caratteri corrispondenti a un modello"
html_title:           "Ruby: Cancellare caratteri corrispondenti a un modello"
simple_title:         "Cancellare caratteri corrispondenti a un modello"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La cancellazione di caratteri corrispondenti a un determinato modello è una delle operazioni comuni nel mondo della programmazione. Si tratta di rimuovere dalla stringa i caratteri che soddisfano un dato criterio, come ad esempio tutti i numeri o tutte le lettere minuscole. I programmatori lo fanno per ottenere stringhe più pulite e compatte, che a loro volta possono essere utilizzate per ulteriori elaborazioni.

## Come fare:
Ecco un semplice esempio di come eliminare tutti i numeri da una stringa utilizzando il metodo "gsub" in Ruby:

```Ruby
stringa = "ci sono 3 gatti nel giardino"
stringa.gsub!(/\d/, "")
puts stringa
```

Questo codice restituirà "ci sono gatti nel giardino" come output. Il metodo "gsub" è utile perché può essere utilizzato con espressioni regolari per rimuovere una vasta gamma di caratteri corrispondenti a un determinato pattern.

## Approfondimento:
La cancellazione di caratteri matching è una soluzione efficiente per gestire stringhe complesse e rimuovere dati superflui. Tuttavia, esistono anche altre opzioni come l'utilizzo del metodo "delete!" che permette di specificare caratteri da eliminare senza l'utilizzo di espressioni regolari. Inoltre, ci sono diverse implementazioni di metodi di eliminazione in diversi linguaggi di programmazione, quindi è importante familiarizzare con la specifica sintassi e funzionalità di ogni linguaggio.

## Vedi anche:
- Documentazione ufficiale di Ruby: https://ruby-doc.org/core-2.6/String.html#method-i-gsub
- Altro approfondimento su come eliminare caratteri matching in Ruby: https://www.rubyguides.com/2019/02/ruby-delete-strings/
- Espressioni regolari in Ruby: https://guides.rubyonrails.org/active_support_core_extensions.html#m-000000