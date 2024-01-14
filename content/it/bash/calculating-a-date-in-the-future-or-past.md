---
title:    "Bash: Calcolare una data nel futuro o nel passato"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Il calcolo di una data nel futuro o nel passato è una delle attività più comuni nei programmatori Bash. Ciò consente di automatizzare i processi che richiedono una determinata data per essere eseguiti, come l'invio di report mensili o il scheduling di attività. Inoltre, il calcolo di una data può essere utile per trovare una scadenza o una data limite in un progetto.

## Come fare

Per calcolare una data nel futuro o nel passato, è necessario utilizzare il comando `date` in Bash. Ecco un esempio di codice che calcola la data di ieri e di domani:

```Bash
# Calculating yesterday's date
yesterday=$(date -d "yesterday" +"%Y-%m-%d")

# Calculating tomorrow's date
tomorrow=$(date -d "tomorrow" +"%Y-%m-%d")

echo "Yesterday's date was: $yesterday"
echo "Tomorrow's date is: $tomorrow"
```

L'output di questo codice sarà:

```Bash
Yesterday's date was: 2021-08-29
Tomorrow's date is: 2021-08-31
```

In questo esempio, abbiamo utilizzato l'opzione `-d` per specificare quali informazioni sulla data vogliamo ottenere da `date`. Possiamo anche utilizzare un formato personalizzato per la data aggiungendo le giuste opzioni. Ad esempio, se volessimo ottenere solo il numero del mese, potremmo utilizzare l'opzione `%m`, come in questo esempio:

```Bash
# Calculating the current month
month=$(date +"%m")

echo "We are in the month number $month"
```

L'output di questo codice sarà:

```Bash
We are in the month number 08
```

Oltre a calcolare date nel futuro o nel passato rispetto alla data attuale, possiamo anche specificare una data di riferimento utilizzando l'opzione `-d` in combinazione con una data. Ad esempio, se volessimo calcolare la data di 10 giorni fa, faremmo:

```Bash
# Calculating a date in the past from a specific date
ten_days_ago=$(date -d "2021-08-20 10 days ago" +"%Y-%m-%d")

echo "10 days ago from 2021-08-20 was $ten_days_ago"
```

L'output di questo codice sarà:

```Bash
10 days ago from 2021-08-20 was 2021-08-10
```

## Approfondimento

Oltre alle opzioni di `date` che abbiamo visto fino ad ora, ci sono molte altre opzioni che possono essere utili per calcolare date in modi diversi. Ad esempio, possiamo utilizzare l'opzione `+%j` per ottenere il numero del giorno dell'anno, o l'opzione `%U` per ottenere il numero della settimana dell'anno. Inoltre, possiamo anche utilizzare `date` per calcolare intervalli di tempo, come ad esempio il numero di giorni tra due date specifiche.

Ricorda che `date` utilizza il formato di data e ora del sistema operativo, quindi potrebbe variare da sistema a sistema. Assicurati di controllare la documentazione del tuo sistema operativo per avere informazioni precise sul formato delle date utilizzato.

## Vedi anche

- [Documentazione su `date` di GNU](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Tutorial su Bash per principianti](https://linuxize.com/post/bash-scripting-tutorial-for-beginners/) 
- [Articolo su come utilizzare `cron` per eseguire script in determinati momenti](https://www.linux.com/topic/desktop/automate-linux-task-using-cron-and-crontab/)