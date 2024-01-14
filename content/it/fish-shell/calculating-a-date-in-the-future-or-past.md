---
title:    "Fish Shell: Calcolare una data nel futuro o nel passato"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per organizzare impegni o visualizzare eventi in un determinato periodo di tempo. In questo articolo, impareremo come usare Fish Shell per eseguire questo tipo di calcoli in modo facile ed efficiente.

## Come fare

Per calcolare la data in futuro o passato, utilizzeremo il comando `date` integrato in Fish Shell. Aggiungendo semplicemente un numero di giorni, mesi o anni al comando, saremo in grado di calcolare una data futura o passata.

```Fish Shell
date -d "now +2 weeks"
date -d "now -1 year"
```

L'output di questi comandi sarà rispettivamente una data che cade due settimane dopo la data attuale e una data che risale a un anno fa.

Per specificare una data precisa, è possibile utilizzare il formato "mese/giorno/anno" oppure "giorno-mese-anno" dopo il comando `date`.

```Fish Shell
date -d "1/1/2022"
date -d "12-31-2021"
```

Questo calcola la data 1° gennaio 2022 o 31 dicembre 2021, a seconda del formato utilizzato.

## Approfondimento

Fish Shell offre anche la possibilità di calcolare una data in base a una data di riferimento specificata. Ad esempio, se vogliamo sapere quale sarà la data tra 100 giorni a partire da oggi, possiamo utilizzare il formato seguente:

```Fish Shell
date -d "now +100 days"
```

Inoltre, è possibile specificare anche orari, minuti e secondi per una maggiore precisione. Ad esempio:

```Fish Shell
date -d "now +5 hours +30 minutes"
```

Calcolerà la data odierna esatta, ma con aggiunta di 5 ore e 30 minuti.

## Vedi anche

- [Documentazione Fish Shell](https://fishshell.com/docs/current/)
- [Comandi utili per Fish Shell](https://fishshell.com/docs/current/cmds.html)
- [Esempi di utilizzo di Fish Shell](https://fishshell.com/docs/current/index.html#examples)