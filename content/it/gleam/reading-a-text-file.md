---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere un file di testo significa interpretare i dati memorizzati in un file in formato leggibile. I programmatori lo fanno per manipolare, interpretare o trasmettere informazioni contenute nei file di testo.

## Come fare:

Ecco come leggere un file di testo in Gleam:

```Gleam
import gleam/otp/file

pub fn main(args: List(String)) {
  let filename = list.head(args) |> result.unwrap(should: "Nessun nome di file dato")
  let text = file.read(filename) |> result.unwrap(should: "Impossibile leggere il file")
  text
    |> string.lines
    |> list.each(fn(line) { io.format("{}", [line]) })
}
```

Il codice sopra prende un nome di file come argomento, legge il file dal disco, poi stampa ogni riga del file.

## Approfondimento

Historicamente, la lettura di un file di testo è stata una delle prime operazioni che i computer erano in grado di fare. Mentre le implementazioni variano da un linguaggio all'altro, il concetto di base è rimasto lo stesso.

Esistono diverse alternative alla lettura di un file di testo, inclusi i database e le API. Tuttavia, i file di testo rimangono un modo semplice e diretto per memorizzare e recuperare dati.

In Gleam, la lettura di un file di testo sfrutta le funzionalità del modulo file dell'OTP (Open Telecom Platform) di Erlang. Questo modulo fornisce un'interfaccia semplice tra il codice Gleam e il sistema di file del sistema operativo.

## Vedi anche

Di seguito alcuni link a risorse correlate che potrebbero interessarti:

2. [Guida pratica Gleam](https://gleam.run/book/tour/)
3. [Documentazione del modulo Erlang OTP file](https://erlang.org/doc/man/file.html)