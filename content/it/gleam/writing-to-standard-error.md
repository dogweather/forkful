---
title:                "Gleam: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivi alla standard error (standard di errore) è un'abilità essenziale per ogni programmatore Gleam. Questo consente di visualizzare facilmente gli errori che si verificano durante l'esecuzione del codice, facilitando così il processo di debugging e rilevamento dei problemi.

## Come Fare

Per scrivere alla standard error in Gleam, è sufficiente utilizzare la funzione `error/1` seguita da una stringa che descrive l'errore. Ad esempio:

```Gleam
error("Errore nel calcolo della media")
```

Questo produrrà un output come questo nella console:

```
ERROR: Errore nel calcolo della media
```

## Approfondimento

Scrivere alla standard error offre ulteriori possibilità, come l'utilizzo di segnaposto `{}` per formattare diversi tipi di dati. Ad esempio:

```Gleam
let errore_divisione = "Errore durante la divisione: {}"

error(errore_divisione("divisione per zero"))
```

Questo produrrà un output come questo:

```
ERROR: Errore durante la divisione: divisione per zero
```

È anche possibile utilizzare la funzione `format/2` per ottenere un controllo più preciso sul formato dell'output della funzione `error/1`. Questa funzione accetta come primo argomento una stringa con il formato desiderato e come secondo argomento una lista di dati che verranno inseriti nel formato. Ad esempio:

```Gleam
let errore_informazioni = "Errore durante il calcolo: {} diviso per {}"

error(format(errore_informazioni, [10, 0]))
```

Questo produrrà un output come questo:

```
ERROR: Errore durante il calcolo: 10 diviso per 0
```

## Vedi Anche

- Documentazione Gleam standard library sulla funzione `error/1`: https://gleam.run/modules/gleam_stdlib/Error
- Esempi di utilizzo della funzione `error/1` su GitHub: https://github.com/gleam-lang/gleam/blob/master/examples/io/src/error.gleam