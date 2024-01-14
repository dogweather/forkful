---
title:                "Ruby: Ottenere la data corrente"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La programmazione è un campo in costante evoluzione e imparare un nuovo linguaggio può sembrare scoraggiante. Ma con l'avvento di Ruby, la programmazione può essere resa più facile e divertente. Una delle attività quotidiane di un programmatore è ottenere la data corrente e, con Ruby, è possibile farlo in modo efficiente e veloce.

## Come fare

Per ottenere la data corrente in Ruby, utilizzeremo il metodo `Time.now`. Vediamo un esempio pratico:

```Ruby
today = Time.now
puts today
```
L'output di questo codice sarà la data e l'ora corrente nel formato `Anno-Mese-Giorno Ore:Minuti:Secondi`. Ad esempio: `2021-05-31 14:35:21`

Il metodo `Time.now` ci restituisce una istanza della classe `Time`, il che significa che possiamo utilizzare tutti i metodi disponibili per questa classe, come ad esempio il metodo `strftime` per formattare la data nel formato che desideriamo. Vediamo un esempio:

```Ruby
today = Time.now
puts today.strftime("%d/%m/%Y")
```
In questo caso, l'output sarà la data corrente nel formato `Giorno/Mese/Anno`. Ad esempio: `31/05/2021`

## Approfondimento

Sapere come ottenere la data corrente è importante, ma è altrettanto importante capire come viene rappresentata e gestita nel linguaggio di programmazione. In Ruby, la data e l'ora sono gestite utilizzando l'oggetto `Time`, che è uno dei tanti tipi di dati disponibili.

La classe `Time` è dotata di molti metodi utili per manipolare e gestire date e orari. Ad esempio, con il metodo `day` possiamo ottenere il giorno del mese, mentre con `month` possiamo ottenere il mese corrente. Esistono anche metodi per aggiungere o sottrarre giorni, mesi o anni da una data specifica.

Inoltre, Ruby offre la possibilità di convertire una stringa in un oggetto `Time` utilizzando il metodo `parse`. Questo è utile quando si ha una data in un formato diverso da quello desiderato e si vuole convertirla in un oggetto `Time` per manipolarla.

## Vedi anche

- [Documentazione di Ruby `Time` class](https://ruby-doc.org/core-3.0.1/Time.html)
- [Tutorial su come gestire le date con Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-control-flow-u/articles/ruby-time-date-manipulation)