---
title:    "Bash: Calcolare una data nel futuro o passato."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti dover calcolare una data nel futuro o nel passato utilizzando il Bash programming. Potresti voler pianificare una data importante, come un compleanno o un'anniversario, o semplicemente avere bisogno di lavorare con date in un progetto di programmazione. Indipendentemente dalla ragione, il calcolo delle date può essere molto utile e può semplificare la tua vita.

## Come fare

Per calcolare una data nel futuro o nel passato utilizzando il Bash programming, ci sono alcuni semplici passaggi che puoi seguire.

1. Definisci la data di base che vuoi utilizzare come punto di partenza. Questo può essere fatto utilizzando il comando `date` seguito dal formato desiderato della data. Ad esempio, per definire la data odierna nel formato `giorno-mese-anno`, puoi digitare `date +%d-%m-%Y`.

2. Aggiungi o sottrai i giorni, i mesi o gli anni desiderati utilizzando il carattere `+` o `-` e il numero corrispondente. Ad esempio, per ottenere la data di oggi più 10 giorni, puoi digitare `date -d "+10 days" +%d-%m-%Y`.

3. Specifica il formato desiderato della data utilizzando il carattere `%` seguito dalle lettere corrispondenti al giorno, al mese e all'anno. Ad esempio, se vuoi ottenere la data nel formato `mese-giorno-anno`, puoi digitare `date -d "+10 days" +%m-%d-%Y`.

Di seguito è riportato un esempio completo di codice che calcola la data di 10 giorni nel futuro:

```
#!/bin/bash

# Definisce la data odierna nel formato 'giorno-mese-anno'
today=$(date +%d-%m-%Y)

# Calcola la data di 10 giorni nel futuro nel formato 'mese-giorno-anno'
future_date=$(date -d "+10 days" +%m-%d-%Y)

# Stampa i risultati
echo "Data odierna: $today"
echo "Data di 10 giorni nel futuro: $future_date"
```

Ecco l'output di questo esempio:

```
Data odierna: 11-10-2021
Data di 10 giorni nel futuro: 21-10-2021
```

## Approfondimento

Oltre ai passaggi semplici sopra descritti, puoi anche eseguire calcoli più complessi utilizzando il Bash programming. Ad esempio, puoi calcolare una data 10 giorni dopo una data specifica o 10 giorni prima di una data specifica, prendendo in considerazione anche il cambio di mese o di anno.

Inoltre, puoi utilizzare il comando `grep` per filtrare e lavorare solo con specifiche parti della data. Ad esempio, puoi usare `grep` per ottenere solo il mese o solo l'anno della data.

## Vedi anche

- [Comandi e utilizzo di date nel Bash programming](https://www.computerhope.com/unix/utime.htm)
- [10 comandi Bash utili per la gestione delle date](https://www.tecmint.com/useful-linux-commands-programs-part-ii/)
- [10 trucchi Bash per la gestione delle date](https://linuxhint.com/bash-date-tricks/)