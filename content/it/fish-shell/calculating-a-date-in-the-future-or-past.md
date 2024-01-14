---
title:                "Fish Shell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Se vi stai chiedendo perché dovresti impegnarti a calcolare una data nel futuro o nel passato, la risposta è semplice: questo può essere estremamente utile quando si lavora con dati e informazioni temporali. Ad esempio, potresti voler sapere quando un evento si è verificato nel passato o quando si verificherà in futuro. Questo tipo di calcolo può essere utile anche per la creazione di promemoria o per l'automatizzazione di task.

## Come fare
Per calcolare una data nel futuro o nel passato utilizzando il Fish Shell, esistono diverse opzioni. La più semplice è utilizzare il comando `date` seguito dalle opzioni `--date` e `-d` per specificare la data desiderata. Ad esempio:

```Fish Shell
date --date "1 day"
```

Questo comando restituirà la data di domani nel formato predefinito del sistema. È anche possibile specificare una data specifica utilizzando il formato `YYYY-MM-DD`. Ad esempio:

```Fish Shell
date --date "2020-12-25"
```
Questa opzione è particolarmente utile per calcolare una data in un formato specifico.

## Approfondimento
Oltre alle opzioni di base offerte dal comando `date`, esistono anche altri strumenti che possono essere utili per calcolare date nel futuro o nel passato. Uno di questi è `jdate`, che consente di specificare date e orari utilizzando un formato simile a quello del calendario giuliano. Ad esempio, per ottenere la data di un mese fa utilizzando `jdate`, si può fare:

```Fish Shell
jdate -a -1 months
```

Ci sono anche diversi moduli di Fish Shell disponibili che offrono funzioni aggiuntive per il calcolo delle date, come ad esempio `strftime`che permette di formattare le date in modo personalizzato.

## Vedi anche
- [Documentazione di 'date' del Fish Shell](https://fishshell.com/docs/current/commands.html#date)
- [Documentazione di 'jdate' del Fish Shell](https://fishshell.com/docs/current/commands.html#jdate)
- [Documentazione di 'strftime' del Fish Shell](https://fishshell.com/docs/current/commands.html#date)