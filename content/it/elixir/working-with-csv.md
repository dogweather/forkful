---
title:                "Elixir: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato alla programmazione con Elixir, lavorare con i file CSV potrebbe essere estremamente vantaggioso. CSV è un formato molto comune per la gestione e lo scambio di dati e imparare come lavorare con esso ti permetterà di acquisire alcune delle capacità fondamentali di Elixir, come la manipolazione dei dati e l'elaborazione parallela.

## Come

Elixir offre una libreria standard molto utile per lavorare con file CSV chiamata `CSV`. Per iniziare, dobbiamo importare la libreria utilizzando `import CSV`. 

Per leggere un file CSV, possiamo utilizzare il metodo `CSV.parse/2` fornendo il percorso del file come primo argomento e le opzioni come secondo argomento. Ad esempio:

```Elixir
result = CSV.parse("path/to/file.csv", [headers: true]) 
```

In questo esempio, abbiamo impostato l'opzione `headers` su `true` per indicare che il file CSV ha una riga di intestazione. Il risultato di `CSV.parse/2` sarà una mappa con le intestazioni come chiavi e i valori corrispondenti nelle righe del file CSV.

Possiamo anche scrivere un file CSV utilizzando il metodo `CSV.encode/2`. Ad esempio:

```Elixir
data = [["nome", "cognome", "età"], ["Marco", "Rossi", 30], ["Laura", "Bianchi", 25]]
CSV.encode(data, [headers: true])
```

In questo caso, abbiamo fornito una lista di liste contenente i dati e l'opzione `headers` per indicare che la prima lista contiene le intestazioni delle colonne. Il risultato sarà una stringa contenente il file CSV formattato correttamente.

## Deep Dive

La libreria CSV di Elixir offre anche molte altre funzionalità utili, come la possibilità di specificare il delimitatore delle colonne, il carattere di citazione e le opzioni per la gestione degli errori. È possibile consultare la documentazione ufficiale per maggiori dettagli e ulteriori opzioni.

Inoltre, Elixir offre anche librerie esterne per lavorare con CSV, come `NimbleCSV` e `CSVix`, che offrono funzionalità avanzate come la lettura/scrittura su stream e la conversione dei dati in altri formati, come JSON.

## Vedi anche

- Documentazione ufficiale di Elixir sulla libreria `CSV`: http://elixir-lang.org/docs/stable/elixir/CSV.html 
- Libreria `NimbleCSV`: https://github.com/plataformatec/nimble_csv
- Libreria `CSVix`: https://github.com/evadne/csvix