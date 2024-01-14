---
title:                "Ruby: Trasformare una data in un testo"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte quando si lavora con dati e informazioni, è necessario rappresentarli in un formato leggibile e visivamente comprensibile. In ambito di programmazione Ruby, a volte ci si trova a dover convertire una data in una stringa, per esempio per mostrarla all'utente o per analizzarla in modo più accurato. In questo articolo vedremo come fare questa operazione in modo semplice ed efficiente.

## Come Fare

Per convertire una data in una stringa in Ruby, si può utilizzare il metodo `strftime`, che sta per "string format time" (formato di data in stringa). Inizialmente, è necessario avere una data in un formato interpretabile da Ruby, ad esempio utilizzando il metodo `parse` su una stringa o utilizzando il costruttore `Date.new` con i valori per l'anno, il mese e il giorno. Una volta ottenuta la data, si può utilizzare `strftime` per convertirla in una stringa utilizzando un formato specifico.

Ecco un esempio di come utilizzare `strftime`:

```Ruby
date = Date.parse("27/10/2021")
puts date.strftime("%d/%m/%Y")
```

In questo caso, il formato `%d/%m/%Y` significa che si vuole che la data venga rappresentata con il giorno, il mese e l'anno, separati da uno slash. Di conseguenza, l'output sarà: `27/10/2021`. Ci sono molti altri formati che si possono utilizzare, come `%b` per rappresentare il mese con le prime tre lettere del suo nome (ad esempio, ottobre diventerà `Oct`) o `%A` per il nome completo del giorno della settimana (ad esempio, mercoledì diventerà `Wednesday`).

## Approfondimento

Il metodo `strftime` offre una grande versatilità nella conversione di date in stringhe, permettendo di utilizzare diversi formati per rappresentare diversi aspetti delle date. Inoltre, è possibile combinare diversi formati all'interno dello stesso `strftime` per ottenere una rappresentazione ancora più precisa. Ad esempio, `%H:%M:%S` rappresenta le ore, i minuti e i secondi di una data in formato 24 ore (ad esempio, `17:09:15`).

Inoltre, si può anche utilizzare `strftime` per operazioni più complesse, come la conversione di una data in un fuso orario specifico utilizzando `%z` oppure per rappresentare il numero di settimane dell'anno utilizzando `%W`.

In generale, è importante essere consapevoli dei numerosi formati e delle possibilità offerte da `strftime` per ottenere la rappresentazione desiderata della data.

## Vedi Anche

- [ruby-doc.org - Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [ruby-doc.org - strftime](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html#method-i-strftime)
- [Tutorial di Ruby on Rails su come formattare la data in una vista](https://guides.rubyonrails.org/v5.2.3/layouts_and_rendering.html#formatting-the-time-and-date)