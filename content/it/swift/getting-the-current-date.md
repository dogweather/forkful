---
title:    "Swift: Ottenere la data corrente."
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Perché

Siamo circondati da tecnologia in ogni aspetto delle nostre vite quotidiane e la data corrente è un dato importante che spesso dobbiamo visualizzare o utilizzare nei nostri programmi. Ecco perché è importante sapere come ottenere la data corrente utilizzando Swift.

## Come

In Swift, possiamo ottenere la data corrente utilizzando la classe `Date`. Ecco un semplice esempio di codice che ci mostra come ottenere la data corrente:

```
let currentDate = Date()
print(currentDate)
```

L'output sarà nella forma di una data e ora, come ad esempio `2021-10-21 12:34:56 +0000`.

Possiamo anche formattare la data in modo diverso utilizzando `DateFormatter`. Ad esempio, se vogliamo solo visualizzare la data nel formato `dd/MM/yyyy`, possiamo utilizzare il seguente codice:

```
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let currentDate = formatter.string(from: Date())
print(currentDate)
```

Il risultato sarà una stringa contenente solo la data, come ad esempio `21/10/2021`.

## Deep Dive

La classe `Date` è parte del framework Foundation di Swift e viene utilizzata per rappresentare date e orari specifici. È importante notare che questa classe rappresenta sempre la data e l'ora in UTC, a meno che non venga specificato un fuso orario diverso.

Inoltre, la classe `Date` è immutabile, il che significa che non possiamo modificare una data già creata. Se vogliamo creare una data con un valore specifico, dobbiamo utilizzare la classe `Calendar` per impostare i componenti come anno, mese, giorno, ora, minuti e secondi.

## Vedi Anche

- Documentazione ufficiale per la classe `Date`: https://developer.apple.com/documentation/foundation/date
- Ulteriori informazioni su come utilizzare `DateFormatter` per formattare le date: https://developer.apple.com/documentation/foundation/dateformatter