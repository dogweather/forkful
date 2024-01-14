---
title:    "Ruby: Ottenere la data corrente"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché:

Ottenere la data corrente all'interno di un programma può essere utile in molte situazioni, come ad esempio per tenere traccia delle attività quotidiane, per visualizzare la data corretta sui documenti o per gestire le scadenze. Utilizzare il linguaggio di programmazione Ruby rende questo processo ancora più semplice e veloce.

## Come fare:

Per ottenere la data corrente in Ruby, è possibile utilizzare il metodo `Time.now` seguito dal metodo `strftime` che permette di formattare la data in modo personalizzato. Ecco un esempio di codice:

```Ruby
puts Time.now.strftime("Oggi è il %d/%m/%Y")
```

Il codice sopra mostrerà l'output "Oggi è il giorno/mese/anno corrente". È possibile personalizzare il formato a piacimento, sostituendo i diversi caratteri con quelli desiderati. Ad esempio, `%b` per il mese abbreviato o `%H` per visualizzare l'ora in formato 24 ore. Possiamo anche ottenere informazioni più specifiche sulla data, come il numero del giorno nella settimana o l'anno, utilizzando altri metodi come `Time.now.wday` o `Time.now.year`.

## Approfondimento:

Ruby offre molti metodi per gestire le date e il tempo, come ad esempio il metodo `mktime` per creare una data specifica o `ctime` per convertire il tempo Unix in un formato leggibile. Inoltre, è possibile utilizzare la gemma "date" per svolgere operazioni più complesse come calcolare la differenza tra due date o ottenere il giorno della settimana corrispondente a una data specifica.

## Vedi anche:

- [Documentazione di Ruby su Time](https://ruby-doc.org/core-2.7.1/Time.html)
- [Gemma "date" di Ruby](https://github.com/ruby/date)
- [Tutorial su come gestire date e tempo in Ruby](https://www.rubyguides.com/2015/12/ruby-time/)