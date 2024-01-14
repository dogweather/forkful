---
title:                "Ruby: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato è un'abilità fondamentale per qualsiasi programmatore Ruby. Questo ti consente di manipolare le date nei tuoi programmi e gestire facilmente le operazioni di tempo.

## Come fare

Per calcolare una data nel futuro o nel passato in Ruby, hai bisogno di usare la libreria di base `Date` e il suo metodo `+` o `-`, passando come parametro il numero di giorni da aggiungere o sottrarre.

Ecco un esempio di codice che calcola la data di oggi più 10 giorni nel futuro:

```Ruby
require 'date'

puts "Oggi è il #{Date.today.strftime('%d/%m/%Y')}"

puts "La data tra 10 giorni sarà il #{(Date.today + 10).strftime('%d/%m/%Y')}"
```

L'output di questo codice sarà:

```
Oggi è il 20/08/2021
La data tra 10 giorni sarà il 30/08/2021
```

Ecco invece un esempio per calcolare la data di oggi meno 5 giorni nel passato:

```Ruby
require 'date'

puts "Oggi è il #{Date.today.strftime('%d/%m/%Y')}"

puts "La data di 5 giorni fa era il #{(Date.today - 5).strftime('%d/%m/%Y')}"
```

L'output di questo codice sarà:

```
Oggi è il 20/08/2021
La data di 5 giorni fa era il 15/08/2021
```

## Approfondimento

Oltre all'aggiunta e alla sottrazione di giorni, la libreria `Date` di Ruby offre molti altri metodi utili per manipolare le date. Ad esempio, puoi usare `next_day` per ottenere la data del giorno successivo, `prev_day` per ottenere la data del giorno precedente, oppure `strftime` per formattare la data secondo le tue preferenze.

Assicurati di consultare la documentazione ufficiale di Ruby per saperne di più sulla manipolazione delle date.

## Vedi anche

- [Documentazione ufficiale di Ruby per il modulo Date](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Una guida dettagliata su come lavorare con le date in Ruby](https://www.rubyguides.com/2018/08/ruby-dates/)