---
title:    "Fish Shell: Confronto tra due date"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo, vedremo come utilizzare il Fish Shell per confrontare due date. Conoscere bene questa funzione è utile per gestire grandi quantità di dati e analizzare l'accesso ai file.

## Come Fare

Per confrontare due date utilizzando il Fish Shell, dobbiamo prima di tutto conoscere il formato delle date supportato da questo shell. Le date possono essere inserite in vari formati, come YYYY/MM/DD, DD/MM/YYYY, YYYY-MM-DD, etc.

Supponiamo di voler confrontare due date, come ad esempio la data di creazione e la data di ultima modifica di un file. Utilizzeremo il comando `stat` per ottenere le informazioni su un file e poi selezioneremo le informazioni desiderate utilizzando i filtri. Vediamo un esempio:

```Fish Shell 
stat -c '%y' file.txt
```
Questa riga di codice ci restituirà la data di ultima modifica di `file.txt` nel formato "YY-MM-DD".

Per confrontare due date, dobbiamo formattarle allo stesso modo. Utilizziamo i filtri `date` per fare ciò. Ad esempio, se volessimo confrontare le date del 10 giugno 2021 e 18 agosto 2021, il codice sarebbe il seguente:

```Fish Shell
date -d "2021-06-10" +%s
date -d "2021-08-18" +%s
```
L'output di questi due comandi sarebbe rispettivamente 1623264560 e 1629212400, in quanto `+%s` ci restituisce il numero di secondi trascorsi dal 1 gennaio 1970 (Unix time).

Per confrontare le due date, possiamo ora utilizzare il comando `test` in combinazione con gli operatori `eq` (uguale), `ne` (diverso), `lt` (minore di), `gt` (maggiore di). Ad esempio:

```Fish Shell
test (date -d "2021-06-10" +%s) -gt (date -d "2021-08-18" +%s)
```
Questo ci restituirà come output `0`, in quanto la prima data è inferiore alla seconda.

## Deep Dive

Come abbiamo visto, il Fish Shell supporta diversi formati di date e ci fornisce un modo semplice e veloce per confrontarle utilizzando i filtri e gli operatori. È inoltre possibile utilizzare altri comandi e filtri, come `awk`, `grep` e `cut` per gestire date più specifiche o per svolgere altre operazioni sui dati.

Un altro aspetto interessante da considerare è il fatto che il Fish Shell è completamente personalizzabile attraverso vari file di configurazione, come `fish.config` e `fish\_functions`. Ciò significa che è possibile implementare funzionalità aggiuntive per il confronto tra date e rendere il proprio lavoro ancora più efficiente.

## Vedi Anche

- Documentazione ufficiale del Fish Shell: <https://fishshell.com/docs/current/index.html>
- Comandi utile per la gestione delle date su Fish Shell: <https://devhints.io/fish-shell>