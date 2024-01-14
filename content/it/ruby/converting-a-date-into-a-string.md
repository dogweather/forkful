---
title:                "Ruby: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa può sembrare un'operazione semplice, ma è un'abilità fondamentale per chiunque stia imparando a programmare in Ruby. Conoscere come gestire le date è importante per creare applicazioni che richiedono un uso accurato del tempo e per presentare le informazioni in un formato più comprensibile per gli utenti.

## Come fare

Per convertire una data in una stringa utilizzando Ruby, è possibile utilizzare il metodo `to_s`. Di seguito è riportato un esempio di codice:

```Ruby
today = Date.today
puts today.to_s
```

L'output di questo codice sarà una stringa che rappresenta la data attuale nel formato `YYYY-MM-DD`. In alternativa, è possibile specificare un formato personalizzato utilizzando il metodo `strftime`:

```Ruby
birthday = Time.new(1990, 8, 12)
puts birthday.strftime("%d/%m/%Y")
```

In questo esempio, vengono utilizzate le direttive del formato comune per stampare la data di nascita nel formato `GG/MM/AAAA`.

## Approfondimento

Oltre ai metodi `to_s` e `strftime`, Ruby offre anche altre opzioni per convertire una data in una stringa. Ad esempio, il metodo `strftime` può accettare una lingua come parametro per stampare il nome del mese o del giorno della settimana nella lingua specificata. Inoltre, è possibile utilizzare il modulo `Date` per manipolare timestamp o per effettuare calcoli di date.

Un altro aspetto da considerare è che le date possono variare in base al fuso orario. Per gestire questa complicazione, Ruby offre il modulo `Timezone` che consente di specificare un fuso orario specifico per le date.

Inoltre, se si lavora con dati sensibili, è importante tenere a mente le diverse convenzioni di formato delle date in tutto il mondo. Per evitare confusioni, è possibile utilizzare i metodi `parse` o `strptime` per convertire una stringa in una data, che poi può essere formattata nel formato desiderato.

## Vedi anche

- [Documentazione ufficiale di Ruby su Date](https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html)
- [Documentazione ufficiale di Ruby su Time](https://ruby-doc.org/core-2.6.3/Time.html)
- [Risorse online per formattare date in Ruby](https://www.rubyguides.com/2015/06/ruby-time/)