---
title:    "Ruby: Calcolare una data nel futuro o nel passato"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti aver bisogno di calcolare una data in futuro o in passato nel tuo programma Ruby. Ad esempio, potresti voler creare una funzione che permetta agli utenti di pianificare eventi futuri, calcolando automaticamente la data in base alle loro preferenze. Inoltre, calcolando una data in passato, potresti creare funzioni che tengano traccia delle scadenze o dei giorni lavorativi.

## Come Fare

Per calcolare una data in futuro o in passato, ci sono alcuni metodi utili che Ruby mette a disposizione.

Per calcolare una data in futuro, puoi utilizzare il metodo `#+` per aggiungere un determinato numero di giorni a una data esistente, come illustrato nell'esempio seguente:

```Ruby
Date.today + 7 # restituisce la data di oggi più 7 giorni
#=> 2021-06-14
```

In modo simile, per calcolare una data in passato, puoi utilizzare il metodo `#-` per sottrarre un numero di giorni da una data, come mostrato nell'esempio qui sotto:

```Ruby
Date.today - 3 # restituisce la data di oggi meno 3 giorni
#=> 2021-06-04
```

Inoltre, puoi anche specificare una data specifica utilizzando il metodo `#new` insieme agli argomenti corrispondenti per l'anno, il mese e il giorno, come in questo esempio:

```Ruby
Date.new(2022, 12, 25) # crea una nuova data per il 25 Dicembre 2022
#=> 2022-12-25
```

## Analisi Approfondita

Oltre ai metodi sopra menzionati, Ruby offre anche la possibilità di utilizzare altri metodi per calcolare una data in futuro o in passato. Alcuni di questi includono `#next_day` e `#prev_day` per ottenere la data del giorno successivo o precedente, `#next_month` e `#prev_month` per ottenere la data del mese successivo o precedente, e `#next_year` e `#prev_year` per ottenere la data dell'anno successivo o precedente.

Inoltre, è possibile specificare l'incremento o il decremento personalizzati utilizzando il metodo `#advance` e specificando le unità di tempo come `:days`, `:months` o `:years` e i relativi valori, come in questo esempio:

```Ruby
Date.today.advance(months: 3, days: 7) # restituisce la data di oggi più 3 mesi e 7 giorni
#=> 2021-09-11
```

Questi sono solo alcuni dei metodi disponibili, ma ci sono molte altre opzioni per calcolare una data in futuro o in passato in Ruby. Assicurati di consultare la documentazione ufficiale per ulteriori informazioni.

## Vedi Anche

- [Documentazione Ruby sulla classe Date](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Tutorial su come utilizzare le date in Ruby](https://www.rubyguides.com/ruby-tutorial/date-time/)
- [Articolo su come calcolare una data in futuro o in passato in Ruby](https://www.rubyguides.com/2018/11/ruby-date-calculate/)