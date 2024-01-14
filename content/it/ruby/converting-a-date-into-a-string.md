---
title:    "Ruby: Convertire una data in una stringa"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'attività comune nella programmazione Ruby. Questo può essere fatto per facilitare la visualizzazione della data in un formato più leggibile o per eseguire operazioni su di essa, come la ricerca o l'ordinamento. In questo articolo vedremo come convertire una data in una stringa utilizzando il linguaggio Ruby.

## Come fare

Per convertire una data in una stringa in Ruby, possiamo utilizzare il metodo `strftime` della classe `Time` o `Date`. Questi metodi permettono di formattare la data in base a un determinato formato fornito come argomento. Ad esempio:

```Ruby
time = Time.now
puts time.strftime("%d/%m/%Y") #=> 12/03/2021
```

In questo esempio, abbiamo utilizzato il formato `%d/%m/%Y` che rappresenta rispettivamente il giorno, il mese e l'anno nella forma numerica. Possiamo utilizzare anche altri formati come `%b %d, %Y` per ottenere una stringa come "Mar 12, 2021". Possiamo anche utilizzare `Date.today` al posto di `Time.now` per ottenere una data senza il componente dell'ora.

## Approfondimento

Il metodo `strftime` accetta una serie di caratteri speciali per formattare la data. Alcuni dei più comuni sono:

- `%Y` per l'anno completo
- `%m` per il mese (01-12)
- `%d` per il giorno del mese (01-31)
- `%b` per il mese abbreviato (Jan-Dec)
- `%B` per il mese completo (January-December)
- `%H` per l'ora (00-23)
- `%M` per i minuti (00-59)
- `%S` per i secondi (00-59)

Ci sono altri caratteri speciali disponibili e un approfondimento sulla loro utilizzo può essere trovato nella documentazione ufficiale di Ruby.

## Vedi anche

- [Documentazione ufficiale di Ruby](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)
- [Tutorial su come formattare una data in Ruby](https://code-maven.com/formatting-a-date-in-ruby)