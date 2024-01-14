---
title:                "Gleam: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si sviluppa un'applicazione, è necessario utilizzare la data corrente come parte della logica del programma. In Gleam, è possibile facilmente ottenere la data corrente utilizzando alcune funzioni built-in.

## Come fare

Per ottenere la data corrente, utilizzeremo la funzione `ctime.now()`. Questo restituirà una struttura di tipo `Time` che rappresenta la data e l'ora attuali. Di seguito è riportato un esempio di codice con un'implementazione base di questa funzione:

```Gleam
import gleam/ctime

pub fn main() {
  let now = ctime.now()
  debug(now)
}
```

L'output di questo codice sarà qualcosa del genere:

```
{time.Second, {2021, March, 10}, {3, 18, 45}}
```

Possiamo anche utilizzare la funzione `ctime.format()` per formattare la data nel formato desiderato. Ad esempio, se vogliamo ottenere la data nel formato "DD/MM/YYYY", possiamo utilizzare questo snippet di codice:

```Gleam
import gleam/ctime

pub fn main() {
  let now = ctime.now()
  let formatted_date = ctime.format("DD/MM/YYYY", now)
  debug(formatted_date)
}
```

Il risultato di questo codice sarà qualcosa del genere:

```
10/03/2021
```

## Approfondimento

La funzione `ctime.now()` utilizza il fuso orario locale del sistema in cui viene eseguito il codice. Se si desidera ottenere la data in un fuso orario specifico, è possibile utilizzare la funzione `ctime.from_utc()` passando come argomenti la data e l'ora desiderate, insieme al fuso orario corrispondente. Ad esempio:

```Gleam
import gleam/ctime

pub fn main() {
  let now = ctime.from_utc({2021, March, 10}, {12, 0, 0}, "America/New_York")
  debug(now)
}
```

Questo restituirà la data nel fuso orario di New York.

## Vedere anche

- Documentazione di Gleam sulle funzioni di data e ora: [https://gleam.run/docs/stdlib/ctime](https://gleam.run/docs/stdlib/ctime)
- Formato delle stringhe di data e ora in Gleam: [https://gleam.run/docs/stdlib/ctime#timeformat](https://gleam.run/docs/stdlib/ctime#timeformat)
- Lista dei fusi orari supportati da Gleam: [https://gist.github.com/noolarch/0779e72b3f5dc4157e17664860ab5459](https://gist.github.com/noolarch/0779e72b3f5dc4157e17664860ab5459)