---
title:                "Gestione degli errori"
date:                  2024-01-26T00:52:09.233297-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Gestire gli errori significa prevedere che nel tuo codice possano verificarsi anomalie e gestire queste situazioni con eleganza. I programmatori fanno questo perché mantiene le applicazioni robuste e facili da usare, anche quando si sono confrontati con l'imprevisto.

## Come fare:
In Gleam, molto spesso si utilizza il tipo `Result` per la gestione degli errori. È un enum con due varianti: `Ok` (per il successo) e `Error` (per il fallimento). Ecco un semplice esempio:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! Si è rotto.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(messaggio) => {
      io.println(messaggio)
      0
    }
  }
}
```

Se esegui `main` con `might_fail(False)`, restituirà `42`. Se passi `True`, stampa "Oops! Si è rotto." e restituisce `0`.

## Approfondimento
L'approccio di Gleam alla gestione degli errori è influenzato dalle sue radici Erlang. Storicamente, Erlang adotta una filosofia del "lascia che si blocchi" (let it crash), facendo affidamento su alberi di supervisione per gestire i fallimenti dei processi. Tuttavia, quando scrivi codice Gleam che non si trova all'interno di un processo destinato a essere supervisionato, come all'interno di una funzione di libreria, vorresti gestire gli errori esplicitamente.

Alternative all'utilizzo di `Result` includono l'uso del tipo `Option` per casi in cui qualcosa potrebbe essere `None` (niente) o `Some` (qualcosa), ma queste non trasportano informazioni sull'errore. Per segnalare errori attraverso i confini dei processi, si potrebbe utilizzare i meccanismi di passaggio di messaggi di Erlang.

La gestione degli errori di Gleam riflette uno stile di programmazione funzionale, dove gli effetti collaterali (come gli errori) vengono gestiti con tipi e pattern-matching, fornendo chiarezza e prevedibilità nella gestione degli errori.

## Vedi Anche
- [Gestione degli Errori in Erlang](http://erlang.org/doc/reference_manual/errors.html)