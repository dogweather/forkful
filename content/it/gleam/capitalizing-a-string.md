---
title:    "Gleam: Convertire in maiuscolo una stringa"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa può sembrare un'operazione semplice e di poco conto, ma in realtà può essere molto utile in diverse situazioni. Ad esempio, potresti voler rendere una stringa completamente maiuscola o minuscola per uniformare il testo o per scopi di confronto, oppure potresti voler capitalizzare il primo carattere di ogni parola in una frase per renderla più leggibile. In ogni caso, la capitalizzazione di una stringa è un'operazione fondamentale nella manipolazione dei dati all'interno di un programma.

## Come Fare

Per capitalizzare una stringa in Gleam, utilizzeremo la funzione `String.capitalize` che prende come argomento la stringa da capitalizzare e restituisce una nuova stringa con il primo carattere maiuscolo e gli altri caratteri in minuscolo. Vediamo un esempio pratico:

```Gleam
let stringa = "ciao a tutti!"
let stringa_capitalizzata = String.capitalize(stringa)
```

In questo modo otterremo la stringa `Ciao a tutti!` come risultato. Per rendere completamente maiuscola o minuscola una stringa, possiamo utilizzare rispettivamente le funzioni `String.uppercase` e `String.lowercase`. Vediamo un altro esempio:

```Gleam
let stringa = "Gleam è un linguaggio di programmazione funzionale"
let stringa_maiuscola = String.uppercase(stringa)
```

In questo caso, otterremo la stringa `GLEAM È UN LINGUAGGIO DI PROGRAMMAZIONE FUNZIONALE` come risultato.

## Approfondimento

Per comprendere meglio il funzionamento della funzione `String.capitalize`, dobbiamo prima capire come Gleam gestisce le stringhe. A differenza di molti altri linguaggi, in Gleam le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Ciò implica che quando viene chiamata la funzione `String.capitalize`, viene creata una nuova stringa con il risultato della capitalizzazione, mentre la stringa originale rimane inalterata.

Un altro aspetto importante da considerare è l'utilizzo della funzione `String.before` per gestire eventuali caratteri speciali all'inizio della stringa, ad esempio un apostrofo in una parola come "l'informatica". Questa funzione restituisce la parte della stringa prima di un determinato carattere o sottostringa specificato. Nel caso dell'esempio precedente, possiamo utilizzarla in questo modo:

```Gleam
let stringa = "l'informatica"
let prima_parte = String.before("'")(stringa)
let seconda_parte = String.after("'")(stringa)
let stringa_capitalizzata = String.capitalize(stringa)
let risultato = prima_parte ++ stringa_capitalizzata ++ seconda_parte
```

Il risultato ottenuto sarà `L'Informatica`, dove la lettera "I" viene opportunamente capitalizzata, mentre le altre rimangono inalterate.

## Vedi Anche

Se vuoi approfondire ulteriormente la manipolazione delle stringhe in Gleam, puoi consultare questi articoli e risorse:

- [Documentazione ufficiale di Gleam sulle stringhe](https://gleam.run/book/std/string.html)
- [Un'introduzione alle stringhe in Gleam](https://gkearney.gitlab.io/gleam-book/part-ii/using-types-1/strings.html)
- [Un tutorial sull'utilizzo di stringhe in Gleam](https://www.tutorialspoint.com/gleam-programming/gleam_programming_strings.htm)