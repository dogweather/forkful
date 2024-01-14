---
title:                "Bash: Ottenere la data corrente."
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe voler ottenere la data corrente all'interno di uno script Bash. Ad esempio, può essere utile per gestire i backup dei file, creare nomi di file unici o semplicemente per visualizzare la data in un formato specifico.

## Come fare

Ci sono diverse modalità di ottenere la data corrente in uno script Bash. Una delle più semplici è utilizzare il comando `date` seguito dalle opzioni del formato desiderato. Ad esempio, ecco come ottenere la data in formato ISO 8601:

```Bash
date +%F
```

Il comando `date` può anche essere utilizzato per specificare una data diversa dalla corrente, ad esempio per ottenere la data di domani:

```Bash
date -d "tomorrow" +%x
```

È anche possibile utilizzare le variabili di sistema per ottenere informazioni sulla data corrente, come ad esempio il giorno della settimana:

```Bash
echo $DATE
```
## Analisi approfondita

Uno dei modi più utili per utilizzare la data corrente è attraverso il comando `touch`. Questo comando può essere utilizzato per assegnare una data specifica ai file, ad esempio per creare un file con una data del futuro. Per fare ciò, si può utilizzare il formato `YYYYMMDDhhmm.ss` per specificare la data e l'ora desiderate, come mostrato nell'esempio seguente:

```Bash
touch -t 202012311200.00 file.txt
```

La data corrente può essere anche utilizzata per creare dei nomi di file unici, ad esempio utilizzando il comando `mv` per rinominare un file aggiungendo la data corrente al suo nome:

```Bash
mv file.txt file_$(date +%y%m%d).txt
```

Ci sono molte altre opzioni e comandi disponibili per gestire la data corrente in uno script Bash, quindi assicurati di fare ricerche approfondite per scoprire quali funzionalità possono essere utili per le tue esigenze.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Tutorial su come utilizzare `date` in Bash](https://www.cyberciti.biz/tips/linux-unix-get-yesterdays-tomorrows-date.html)
- [Domande frequenti su `date` in Bash](https://unix.stackexchange.com/questions/331538/how-to-use-date-command-in-bash)