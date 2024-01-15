---
title:                "Convertire una data in una stringa"
html_title:           "Elixir: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
La conversione di una data in una stringa può essere utile per visualizzare le informazioni della data in un formato specifico o per l'elaborazione dei dati in vari sistemi. Inoltre, in Elixir, la conversione in una stringa può semplificare l'interazione con altri moduli e librerie.

## Come fare
Per convertire una data in una stringa in Elixir, è possibile utilizzare la funzione `DateTime.to_iso8601/2`, passando la data come primo argomento e il formato desiderato come secondo argomento. Ad esempio:

```Elixir
DateTime.to_iso8601(~N[2021-04-15T10:22:00], extended: true)

# Output: "2021-04-15T10:22:00"
```

In questo caso, il secondo argomento `extended: true` specifica che il formato della stringa deve includere anche i secondi e i fusi orari. È possibile anche utilizzare il formato `:compact` per ottenere una stringa con meno caratteri, ma meno dettagliata.

Se si desidera invece convertire la data in un formato personalizzato, è possibile utilizzare la funzione `DateTime.to_string/3`, specificando la data, il formato e la località. Ad esempio:

```Elixir
DateTime.to_string(~N[2021-04-15T10:22:00], "{0,0}-{1}/{2}/{3} {4}:{5}", "it")

# Output: "15-04-2021 10:22"
```

In questo caso, il formato specificato viene interpretato come "giorno-mese-anno ora:minuti" e la località impostata come "it" garantisce che i nomi dei mesi e dei giorni della settimana vengano visualizzati in italiano. È possibile consultare la documentazione di Elixir per ulteriori opzioni di formattazione.

## Approfondimento
Di base, una data in Elixir viene rappresentata come una tupla di sette elementi, contenente l'anno, il mese, il giorno, l'ora, i minuti, i secondi e i fusi orari. Quando viene convertita in una stringa, la data viene formattata seguendo lo standard ISO 8601, che garantisce l'uniformità e la leggibilità tra diversi sistemi.

È importante notare che, in Elixir, le date sono immutabili: ogni operazione di modifica su una data restituisce sempre una nuova data. Questo garantisce la consistenza e l'affidabilità delle informazioni e previene eventuali errori di modifica accidentale.

## Vedi anche
- [Documentazione ufficiale di Elixir sulla conversione di date](https://hexdocs.pm/elixir/DateTime.html#to_string/3)
- [Tutorial su come lavorare con le date in Elixir](https://www.tutorialspoint.com/elixir/elixir_working_with_dates.htm)