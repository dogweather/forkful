---
title:    "Fish Shell: Convertire una data in una stringa"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Se stai programmando con Fish Shell, potresti trovare la necessità di convertire una data in una stringa. Ciò può essere utile per una varietà di scopi, come l'organizzazione dei dati o la creazione di report.

## Come fare

Per convertire una data in una stringa in Fish Shell, è possibile utilizzare il comando `date`. Ad esempio, se vogliamo ottenere la data attuale in formato stringa, possiamo utilizzare il seguente comando:

```Fish Shell
date -f "%Y-%m-%d" 
# Output: 2021-01-27
```

Possiamo anche specificare il formato desiderato utilizzando i caratteri speciales `+%` seguiti da una lettera. Ad esempio, se vogliamo ottenere la data attuale con l'aggiunta del nome del mese, possiamo utilizzare il comando:

```Fish Shell
date +%d %B %Y 
# Output: 27 January 2021
```

## Approfondimento

C'è una varietà di opzioni che possiamo utilizzare con il comando `date` per ottenere diverse combinazioni di data e ora. Ad esempio, possiamo utilizzare il carattere speciale `%H:%M` per ottenere solo l'ora corrente e minuti.

Inoltre, è possibile specificare una specifica data o ora da convertire invece di utilizzare quella attuale. Per fare ciò, basta aggiungere il parametro `-d` seguito dalla data o ora desiderata. Ad esempio:

```Fish Shell
date -d "2021-02-14" +%A 
# Output: Sunday
```

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Tutorial su come utilizzare il comando "date" in Fish Shell](https://www.baeldung.com/linux/fish-shell-date-command) 
- [Altre informazioni su come manipolare le date in Fish Shell] (https://fishery.run/)