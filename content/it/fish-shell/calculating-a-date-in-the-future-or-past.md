---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Fish Shell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 
Calcolare una data futura o passata significa determinare una data spostando un certo numero di giorni prima o dopo una data di partenza specificata. I programmatori lo fanno per gestire eventi temporali, come scadenze, scadenze o programmazione di eventi.

## Come fare:
Ecco un esempio semplice per calcolare una data 5 giorni dopo la data corrente nel fish shell:

```Fish Shell
set -lx data_corrente (date -u +"%Y%m%d")
set -lx data_futura (date -u -d "$data_corrente + 5 days" +"%Y%m%d")
echo $data_futura
```

Questo produrrà un output simile al seguente:

```Fish Shell
20291010
```

## Approfondimento
Mentre la gestione delle date può sembrare semplice, in realtà ha un contesto storico complesso. Il calendario che utilizziamo oggi è frutto di secoli di evoluzione e ha una serie di anomalie. Per gestirli, i programmatori utilizzano una varietà di librerie e strumenti di calcolo delle date.

Ci sono molte alternative al calcolo delle date. Ad esempio, puoi utilizzare librerie di programmazione come la libreria di date e tempo di Java.

Durante il calcolo delle date passate e future, Fish eseguirà la sottrazione o l'aggiunta delle date internamente. Sappi solo che il calcolo viene effettuato in termini di secondi da un'epoca, il 1 gennaio 1970. 

## Vedi Anche
Per ulteriori informazioni su come calcolare le date future o passate, consulta i seguenti link:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)

Ricorda che le capacità di calcolo della data variano da un sistema operativo all'altro e da un linguaggio di programmazione all'altro. Fai attenzione a queste differenze durante la programmazione.