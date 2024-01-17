---
title:                "Calcolare una data nel futuro o nel passato."
html_title:           "Ruby: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Calcolare una data nel futuro o nel passato è semplicemente una funzione che ti permette di ottenere una data dall'oggi avanti o indietro nel tempo. I programmatori lo fanno spesso per eseguire operazioni come aggiornare un'emissione, calcolare le date di scadenza o impostare trigger temporali.

## Come fare:
```
Ruby Date Library:
require 'date'
Date.today + 7 # output: 2020-07-30
```
Puoi usare la libreria predefinita di Ruby per eseguire calcoli di date. In questo esempio, stiamo aggiungendo 7 giorni alla data odierna, ottenendo come risultato la data del 30 luglio 2020. Puoi anche passare un parametro negativo per ottenere una data nel passato.

```
Date calculation using Time:
time = Time.now 
time + (60 * 60 * 24 * 7) # output: 1596510224
```
Puoi anche utilizzare l'oggetto `Time` per eseguire calcoli di date. In questo esempio, stiamo aggiungendo 7 giorni al tempo corrente, ottenendo come risultato un valore numerico che rappresenta i secondi trascorsi dalla mezzanotte del 1 gennaio 1970 (epoca unix).

## Approfondimento:
La necessità di calcolare date è sempre stata presente nella programmazione, soprattutto per la gestione di scadenze e trigger temporali. In passato, i linguaggi di programmazione come C e Java richiedevano l'uso di funzioni complesse per effettuare questi calcoli, ma grazie a linguaggi moderni come Ruby, è diventato molto più semplice e intuitivo.

Esistono anche altre opzioni per eseguire calcoli di date, come l'utilizzo di librerie esterne come [Chronic](https://github.com/mojombo/chronic) o [Natty](https://github.com/withelmo/Natty). Inoltre, puoi anche utilizzare metodi matematici per eseguire calcoli di date manualmente, ma ciò richiederebbe più codice e sarebbe meno efficiente rispetto all'utilizzo delle librerie native di Ruby.

Per quanto riguarda l'implementazione, la libreria Date di Ruby utilizza l'algoritmo di Gauss per gestire le eccezioni nei calcoli delle date, come gli anni bisestili. Inoltre, è in grado di supportare un'ampia gamma di formati di date e orari.

## Vedi anche:
- [Documentazione della libreria Date di Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Guida su come utilizzare la libreria Date di Ruby](https://www.rubyguides.com/2015/08/ruby-date/) 
- [Esempi pratici di utilizzo della libreria Date di Ruby](https://www.theodinproject.com/paths/full-stack-ruby-on-rails/courses/ruby-programming/lessons/ruby-programming#section-calculating-dates-with-the-default-library)