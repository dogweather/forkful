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
Calcolare una data nel futuro o nel passato è un'operazione comune per i programmatori. Questo permette loro di effettuare calcoli temporali per creare funzionalità come notifiche di scadenze o promemoria di eventi.

## Come fare:
```Fish Shell``` offre diverse funzioni e strumenti per calcolare date nel futuro o nel passato. Uno dei modi più semplici per farlo è utilizzare il comando ```date``` seguito dalla data desiderata nel formato ```+days```, dove "days" rappresenta il numero di giorni dopo o prima la data corrente.

Ad esempio, se vogliamo sapere quale data sarà tra 10 giorni, possiamo eseguire il seguente comando:

```
date +10 days
```

Il risultato dovrebbe essere qualcosa del genere:

```
gio 11 ott 2021 10:30:45 CEST
```

Inoltre, è possibile combinare questa funzione con altre come ad esempio ```+weeks```, ```+months``` o ```+years``` per effettuare calcoli ancora più precisi.

## Approfondimento:
In passato, per calcolare date nel futuro o nel passato era necessario utilizzare linguaggi come Ruby o Python. Tuttavia, grazie alla potenza e alla flessibilità di ```Fish Shell```, ora è possibile utilizzarlo anche per questo scopo.

Inoltre, oltre al comando ```date```, esistono anche altri strumenti utili come il plugin [Oh My Fish](https://github.com/oh-my-fish/oh-my-fish) che offre funzioni di calcolo avanzate per le date.

## Vedi anche:
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Plugin Oh My Fish](https://github.com/oh-my-fish/oh-my-fish)
- [Articolo su come calcolare date in Fish Shell](https://www.tecmint.com/set-date-and-time-in-fish/)