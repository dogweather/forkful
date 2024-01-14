---
title:                "Fish Shell: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molti potrebbero chiedersi perché dovrebbero interessarsi a ottenere la data corrente utilizzando il Fish Shell. La risposta è semplice: la data corrente è un elemento fondamentale di qualsiasi programma o script che richieda l'utilizzo del tempo come variabile. Ad esempio, potresti voler creare un file con la data corrente nel nome, o calcolare l'età di una persona in base alla data di nascita. Inoltre, conoscere la data corrente può essere molto utile per l'organizzazione dei tuoi file o per tenere traccia delle tue attività giornaliere.

## Come ottenere la data corrente con il Fish Shell

Per ottenere la data corrente usando il Fish Shell, puoi utilizzare il comando `date` seguito da una formattazione specifica. Ad esempio, per ottenere la data corrente nel formato "mese/giorno/anno", puoi utilizzare il comando:

```
date +%m/%d/%y
```

Il simbolo `%` seguito da una lettera specifica il formato della data da ottenere. Ad esempio, `%m` indica il numero di mese, `%d` il numero di giorno e `%y` l'anno in formato abbreviato a due cifre. Puoi consultare la documentazione del comando `date` per vedere tutti i possibili formati disponibili.

Un'altra opzione è utilizzare il comando `echo` combinato con il comando `date`. Ad esempio, per ottenere la data corrente nel formato "giorno della settimana, giorno/mese/anno", puoi utilizzare il seguente comando:

```
echo (date +%A), (date +%d/%m/%y)
```

Il comando `echo` serve per visualizzare il risultato ottenuto dal comando `date`.

## Deep Dive

Per coloro che sono interessati a saperne di più su come ottenere la data corrente con il Fish Shell, ecco alcune informazioni aggiuntive. Il comando `date` è parte del pacchetto Coreutils, che è disponibile in vari sistemi operativi, tra cui Linux e macOS. Puoi anche utilizzare il comando `man date` per accedere alla documentazione del comando e scoprire tutte le sue opzioni e formati.

Inoltre, è possibile combinare il comando `date` con altri comandi del Fish Shell per ottenere risultati più complessi. Ad esempio, puoi utilizzare il comando `set` per assegnare la data corrente a una variabile e utilizzarla in seguito nel tuo script.

## Vedi anche

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Guide to command substitution in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_expansion)
- [Mastering the date command in Linux](https://www.tecmint.com/20-practical-examples-of-date-command-in-linux/) (in inglese)