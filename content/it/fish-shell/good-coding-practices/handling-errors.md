---
date: 2024-01-26 00:52:22.480055-07:00
description: "La gestione degli errori consente al tuo script di affrontare l'inaspettato\
  \ con eleganza. Lo si fa per gestire i fallimenti senza far diventare i capelli\u2026"
lastmod: '2024-03-13T22:44:43.866700-06:00'
model: gpt-4-1106-preview
summary: La gestione degli errori consente al tuo script di affrontare l'inaspettato
  con eleganza.
title: Gestione degli errori
weight: 16
---

## Cos'è e perché?
La gestione degli errori consente al tuo script di affrontare l'inaspettato con eleganza. Lo si fa per gestire i fallimenti senza far diventare i capelli dei nostri utenti grigi.

## Come fare:
Per intercettare errori in Fish, affidati al comando `status` e alle condizionali. Diciamo che `ping` fallisce; ecco come rilevarlo:

```fish
ping -c 1 example.com
if not status is-success
    echo "Qualcosa di strano è successo con il ping."
end
```

Output di esempio se `ping` fallisce:

```
Qualcosa di strano è successo con il ping.
```

Per gestire un codice di errore specifico, usa `status --is`:

```fish
false
if status --is 1
    echo "Interceptato un errore con codice 1."
end
```

Output di esempio:
```
Interceptato un errore con codice 1.
```

Per un approccio più robusto, considera l'uso di una funzione:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping fallito con stato $status"
        return 1
    end
end

try_ping
```

## Approfondimento
La gestione degli errori in Fish non corrisponde al paradigma `try/catch` che potresti conoscere dai linguaggi di programmazione di alto livello. Invece, si ha a disposizione un'uscita diretta degli stati fornita dal comando `status`.

Storicamente, nei sistemi di tipo Unix, uno stato di uscita di `0` significa successo, mentre qualsiasi valore diverso da zero indica un errore, che comunemente riflette diverse ragioni di fallimento. Questa convenzione è impiegata dalla maggior parte delle utilità da riga di comando e quindi, anche da Fish stesso.

Alternative ai controlli dello stato con `status` in Fish includono la gestione dei segnali tramite `trap` in altre shell, ma Fish preferisce controlli di stato più espliciti, perché sono più puliti e meno soggetti ad effetti collaterali.

Dal punto di vista dell'implementazione, la gestione degli errori in Fish rimane semplice ma potente, in gran parte grazie alla sua natura non bloccante e all'enfasi su una sintassi chiara, come mostrato negli esempi. I codici di errore si integrano bene con le funzioni, consentendo una gestione dell'errore modulare e leggibile.

## Vedi anche
- Documentazione di Fish sulle condizionali: https://fishshell.com/docs/current/language.html#conditionals
- Tutorial di Fish sulla gestione degli errori: https://fishshell.com/docs/current/tutorial.html#error-handling
