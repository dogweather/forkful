---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Prendere la data corrente in programmazione significa ottenere istantaneamente la data e l'ora del sistema. Lo facciamo per segnare eventi, registrare le transazioni o semplicemente visualizzare l'ora e la data attuali.

## Come si fa:

Iniziamo con un esempio di codice Gleam per ottenere la data corrente. Nota: Gleam utilizza il modulo Erlang :calendar per gestire la data e l'ora.

```gleam
import erlang

fn today() {
  let erlang.Tuple(_, month, day) = erlang.localtime()
  day
}
```

Quando esegui questo codice otterai il giorno corrente.

```shell
gleam run today
-> 15 (se oggi è il 15esimo giorno del mese)
```

## Approfondimento

Storicamente, molti linguaggi di programmazione si affidano a librerie o moduli built-in per l'ottenimento della data corrente. Ad esempio, in Gleam utilizziamo il modulo :calendar di Erlang, come abbiamo mostrato sopra.

Ci sono alternative a questa implementazione. Alcuni linguaggi ti permettono di creare il tuo modulo per gestire il tempo e la data. Tuttavia, in Gleam, probabilmente troverai più facile ed efficiente utilizzare le funzioni di Erlang come ```erlang.localtime()```.

## Vedi anche

Per approfondire la gestione delle date e delle ore in Gleam (e in Erlang), dai un'occhiata a queste fonti:

- Documentazione ufficiale di Gleam: https://gleam.run/book/tour/dynamic-typing.html
- Documentazione ufficiale di Erlang :calendar: https://erlang.org/doc/man/calendar.html