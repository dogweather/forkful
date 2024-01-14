---
title:    "Ruby: Ricerca e sostituzione di testo"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono fondamentali per gestire e manipolare i dati in un programma Ruby. Ciò può aiutare a risparmiare tempo ed evitare errori manuali durante l'editing del codice. Imparare come utilizzare questa funzionalità ti consentirà di diventare un programmatore Ruby più efficiente e produttivo.

## Come Fare

Per eseguire una ricerca e sostituzione di testo in Ruby, puoi utilizzare il metodo `.gsub()`. Questo metodo accetta due argomenti: il primo è la stringa di ricerca e il secondo è la stringa di sostituzione. Ecco un esempio:

```Ruby
stringa = "Ciao a tutti!"
stringa.gsub("Ciao", "Salve")
```

In questo esempio, il metodo `.gsub()` sostituirà ogni occorrenza della stringa "Ciao" con la stringa "Salve", producendo il seguente output: "Salve a tutti!".

Puoi anche utilizzare espressioni regolari per eseguire ricerche e sostituzioni più complesse. Ad esempio, se vuoi sostituire tutti i numeri in una stringa con il carattere "*":

```Ruby
stringa = "123 ciao 456"
stringa.gsub(/\d+/, "*")
```

Questo produrrà un output del genere: "* ciao *".

## Approfondimento

Il metodo `.gsub()` è solo una delle molte funzionalità di ricerca e sostituzione disponibili in Ruby. Puoi anche utilizzare il metodo `.sub()` per sostituire solo la prima occorrenza della stringa di ricerca, o il metodo `.gsub!()` per modificare direttamente la stringa originale. Inoltre, puoi utilizzare le opzioni dei metodi di sostituzione per specificare l'uso di letterali al posto di spazi vuoti o modificare la modalità di confronto delle stringhe.

Inoltre, puoi utilizzare espressioni regolari avanzate per eseguire ricerche e sostituzioni più sofisticate, come la ricerca di parole specifiche o la sostituzione basata su uno schema determinato. Ci sono anche molti pacchetti e gemme disponibili per semplificare e potenziare le tue capacità di ricerca e sostituzione.

## Vedi Anche

- [Documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/documentation/)
- [Un'approfondita guida alla ricerca e sostituzione in Ruby](https://www.rubyguides.com/2019/05/ruby-regex/)