---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:12.822597-07:00
description: 'Come fare: In Fish Shell, puoi scrivere su stderr reindirizzando il
  tuo output utilizzando `>&2`. Ecco un esempio base.'
lastmod: '2024-03-13T22:44:43.875760-06:00'
model: gpt-4-0125-preview
summary: In Fish Shell, puoi scrivere su stderr reindirizzando il tuo output utilizzando
  `>&2`.
title: Scrivere sull'errore standard
weight: 25
---

## Come fare:
In Fish Shell, puoi scrivere su stderr reindirizzando il tuo output utilizzando `>&2`. Ecco un esempio base:

```fish
echo "Questo è un messaggio di errore" >&2
```

Questo comando semplicemente fa eco a un messaggio su stderr invece che su stdout. Se dovessi scrivere uno script che produce sia messaggi regolari che di errore, potresti fare qualcosa di simile a questo:

```fish
echo "Avviamento del processo"
echo "Si è verificato un errore" >&2
echo "Processo completato"
```

Output esemplificativo se esegui lo script e reindirizzi stderr in un file:

```
Avviamento del processo
Processo completato
```

Il messaggio di errore non comparirebbe nell'output standard ma si troverebbe nel file a cui hai reindirizzato stderr.

In scenari che richiedono gestione degli errori o registrazione più sofisticata, Fish non viene fornito con librerie integrate progettate espressamente per questo. Tuttavia, puoi sfruttare strumenti esterni o scrivere funzioni per assistere. Ad esempio, creare una semplice funzione di registrazione potrebbe sembrare così:

```fish
function log_error
    echo $argv >&2
end

log_error "Questo è un messaggio di errore avanzato"
```

Questa funzione `log_error` prenderà qualsiasi stringa gli fornisci e la scriverà su stderr. Utilizzare funzioni come questa può aiutare a mantenere la gestione degli errori pulita e coerente in tutto i tuoi script.
