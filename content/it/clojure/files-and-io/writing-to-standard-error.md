---
aliases:
- /it/clojure/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:43.697794-07:00
description: "Scrivere sull'errore standard (stderr) riguarda l'indirizzare messaggi\
  \ di errore e diagnostiche allo stream stderr, separato dall'output standard\u2026"
lastmod: 2024-02-18 23:08:55.576360
model: gpt-4-0125-preview
summary: "Scrivere sull'errore standard (stderr) riguarda l'indirizzare messaggi di\
  \ errore e diagnostiche allo stream stderr, separato dall'output standard\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere sull'errore standard (stderr) riguarda l'indirizzare messaggi di errore e diagnostiche allo stream stderr, separato dall'output standard (stdout). I programmatori fanno questo per differenziare l'output regolare del programma dai messaggi di errore, consentendo un debugging e un logging più efficaci.

## Come fare:
In Clojure, puoi scrivere su stderr utilizzando lo stream `*err*`. Ecco un esempio basilare:

```clojure
(.write *err* "Questo è un messaggio di errore.\n")
```

Nota che dopo aver scritto un messaggio, dovresti fare il flush dello stream per assicurarti che il messaggio venga immediatamente emesso:

```clojure
(flush)
```

Esempio di output su stderr:
```
Questo è un messaggio di errore.
```

Se stai gestendo eccezioni, potresti voler stampare le tracce dello stack su stderr. Usa `printStackTrace` per questo:

```clojure
(try
  ;; Codice che potrebbe generare un'eccezione
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Per un logging degli errori più strutturato, librerie di terze parti come `timbre` possono essere configurate per registrare su stderr. Ecco una configurazione e un uso basilari:

Prima di tutto, aggiungi `timbre` alle tue dipendenze. Poi configuralo per utilizzare stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Disabilita il logging su stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Disabilita il logging su file
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Abilita stderr per gli errori

(timbre/error "Si è verificato un errore durante l'elaborazione della tua richiesta.")
```

Ciò dirigerà i messaggi di livello errore su stderr, rendendoli distinti dall'output standard dell'applicazione.
