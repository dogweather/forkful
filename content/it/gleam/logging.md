---
title:                "Registrazione Eventi (Logging)"
date:                  2024-01-26T01:03:17.954962-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/logging.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Il logging è essenzialmente il modo in cui registriamo ciò che accade nei nostri programmi. È come avere una piccola scatola nera; quando le cose vanno male (e credimi, succederà), i log sono indispensabili per capire cosa è successo, diagnosticare i problemi e ottimizzare le prestazioni.

## Come fare:
In Gleam, di solito si integra una libreria di logging—non c'è un meccanismo di logging dedicato di base. Diciamo che stiamo utilizzando una ipotetica crate `gleam_logger`. Ecco come potresti integrarla:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("L'applicazione sta partendo!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Calcolo eseguito con successo", value)
    Error(err) -> 
      gleam_logger.error("Calcolo fallito", err)
  }
}
```

L'output atteso nei tuoi log dovrebbe assomigliare a questo:

```
INFO: L'applicazione sta partendo!
DEBUG: Calcolo eseguito con successo 42
ERROR: Calcolo fallito Causa: Divisione per zero
```

## Approfondimento
L'arte del logging esiste sin dai primi giorni della programmazione. Gli operatori di sistema ottenevano letteralmente log dal computer - assicurandosi che tutto funzionasse senza intoppi. Andando avanti veloci, il logging è diventato digitale, divenendo una parte fondamentale dello sviluppo software.

Mentre Gleam, essendo un linguaggio relativamente giovane che si rivolge all'ecosistema Erlang, non ha un framework di logging incorporato, puoi sfruttare le mature infrastrutture di logging di Erlang o altre librerie fornite dalla comunità. Ognuna ha caratteristiche e compromessi diversi: alcune potrebbero fornire logging strutturato, altre sono più per l'output di testo semplice.

Ora, la questione dell'implementazione di un sistema di logging: È semplice? A prima vista, sì. Ma se vai a fondo, devi affrontare la gestione della concorrenza, i colli di bottiglia dell'I/O, la rotazione dei log, la standardizzazione dei formati (pensa al JSON per il logging strutturato), il filtraggio dei livelli e, possibilmente, il tracciamento distribuito. Inoltre, in un paradigma funzionale, generalmente si desidera che gli effetti collaterali (come il logging) siano gestiti in modo prevedibile e controllato.

## Vedi Anche
Qui puoi trovare maggiori informazioni sui dettagli del logging in Gleam e il suo ecosistema circostante:
- [Documentazione di :logger di Erlang](http://erlang.org/doc/apps/kernel/logger_chapter.html): Poiché Gleam compila in Erlang, questa è direttamente applicabile.
- [Documenti della libreria standard di Gleam](https://hexdocs.pm/gleam_stdlib/): Per aggiornamenti su eventuali utility di logging che potrebbero essere aggiunte.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Una lista curata di risorse, che potrebbe includere librerie di logging non appena diventano disponibili.
