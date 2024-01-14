---
title:    "Ruby: Ottenere la data corrente"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Perché utilizzare la funzione di data corrente in Ruby?

Se stai sviluppando un'applicazione o un sito web in Ruby, la possibilità di ottenere la data corrente è fondamentale. Con la funzione di data corrente, puoi facilmente mostrare la data e l'orario attuali per fornire informazioni in tempo reale agli utenti del tuo progetto. Inoltre, la data corrente può essere utile per attivare azioni in base al giorno o all'orario, come inviare un promemoria o una notifica programmata.

## Come ottenere la data corrente in Ruby

Per ottenere la data corrente in Ruby, puoi utilizzare il metodo `Time.now`, che restituirà un oggetto di tipo `Time` con la data e l'orario attuali. Ecco un esempio di codice:

```Ruby
Time.now #=> 2021-06-30 15:45:00 +0200
```

Puoi anche specificare una zona oraria specifica utilizzando il metodo `Time.zone.now`, che restituirà un oggetto di tipo `ActiveSupport::TimeWithZone`. Ecco un esempio di codice:

```Ruby
Time.zone.now #=> 2021-06-30 09:45:00 -0400
```

Se vuoi ottenere solo la data corrente senza l'orario, puoi utilizzare il metodo `Date.today`, che restituirà un oggetto di tipo `Date`. Ecco un esempio di codice:

```Ruby
Date.today #=> 2021-06-30
```

## Approfondimento sulle funzioni della data corrente in Ruby

Oltre ai metodi mostrati sopra, Ruby offre molte altre opzioni per manipolare e formattare la data corrente. Ad esempio, puoi utilizzare i metodi `strftime` e `strptime` per convertire la data in una stringa formattata secondo le tue esigenze. Inoltre, puoi utilizzare i metodi `day`, `month` e `year` per ottenere il giorno, il mese e l'anno dalla data corrente. Ecco un esempio di codice:

```Ruby
date = Date.today #=> 2021-06-30
date.strftime("%d/%m/%Y") #=> "30/06/2021"
date.day #=> 30
date.month #=> 6
date.year #=> 2021
```

Puoi anche utilizzare la gemma `Chronic` per gestire in modo più flessibile le date e gli orari, consentendoti di analizzare ed elaborare date in formati diversi. Ecco un esempio di codice:

```Ruby
require 'chronic'
Chronic.parse("next Monday") #=> 2021-07-05 12:00:00 +0200
Chronic.parse("tomorrow at 6pm") #=> 2021-07-01 18:00:00 +0200
```

## Vedi anche

- Documentazione ufficiale di Ruby su Date e Time: https://ruby-doc.org/stdlib-2.4.1/libdoc/date/rdoc/Date.html
- Documentazione ufficiale di Rails su ActiveSupport::TimeWithZone: https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html
- Gemma Chronic per la gestione delle date e degli orari: https://github.com/mojombo/chronic