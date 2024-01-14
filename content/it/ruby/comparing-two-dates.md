---
title:                "Ruby: Confrontare due date"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'attività comune nel mondo della programmazione, specialmente quando si lavora con dati temporali. È importante essere in grado di confrontare le date in modo accurato per garantire che il codice funzioni correttamente.

## Come fare

Per confrontare due date in Ruby, è possibile utilizzare il metodo `compare` della classe `Time`. Questo metodo restituirà un intero negativo se la prima data è precedente alla seconda, un intero positivo se la prima data è successiva alla seconda e 0 se le due date sono uguali.

```
Ruby
now = Time.now
later = Time.now + 60 # Aggiunge 60 secondi
puts now.compare(later) # Output: -1
puts later.compare(now) # Output: 1
puts now.compare(now) # Output: 0
```

## Approfondimento

Comparare due date può essere più complicato di quanto sembri a prima vista. Ci sono vari fattori da considerare, come il fuso orario, l'uso di date con o senza orario, e la conversione tra date di diversi formati. È importante essere consapevoli di questi dettagli per evitare errori nel codice.

## Vedi anche

- [Documentazione ufficiale di Ruby su Time](https://ruby-doc.org/core-2.7.1/Time.html)
- [Tutorial su come confrontare date in Ruby](https://www.rubyguides.com/2015/09/ruby-compares-dates/)
- [Articolo su come gestire date e orari in Ruby](https://www.sitepoint.com/dates-and-times-ruby/)