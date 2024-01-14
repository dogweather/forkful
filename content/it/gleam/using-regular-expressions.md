---
title:    "Gleam: Utilizzo delle espressioni regolari"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o uno sviluppatore web, molto probabilmente hai sentito parlare di regular expressions o espressioni regolari. Queste sono sequenze di caratteri che ti permettono di cercare e manipolare testi in modo efficiente. Ma perché dovresti usarle? In breve, le regular expressions sono uno strumento potente per trovare e sostituire testo in una stringa e possono aiutarti a risparmiare tempo e sforzi nei tuoi progetti di programmazione.

## Come usare le regular expressions in Gleam

Se vuoi imparare come utilizzare le regular expressions in Gleam, sei nel posto giusto! Cominciamo con un esempio semplice: vogliamo trovare tutte le parole che iniziano con la lettera "c" in una stringa. Per farlo, dobbiamo utilizzare la funzione `Regex.match()` e fornirla con la regola di ricerca e la stringa su cui applicarla. Vediamo un esempio pratico:

```Gleam
import gleam/regex

let regex = Regex.match("c[a-z]*", "Gusto italiano per il coding")
Match.regex(regex) == Ok(["coding"])
```

In questo esempio, abbiamo specificato la regola "c[a-z]*", che indica che la parola deve iniziare con "c" seguito da qualsiasi numero di lettere minuscole. Il risultato è una lista con la parola che ha trovato corrispondenza, nel nostro caso "coding".

## Approfondimento sulle regular expressions

Ora che hai visto un esempio pratico di come utilizzare le regular expressions in Gleam, puoi approfondire il tuo studio su questo argomento. Ci sono molte funzioni disponibili nella libreria `Regex` di Gleam, come ad esempio `split()`, `replace()` e `find()`, che puoi utilizzare per effettuare ricerche ancora più complesse. Inoltre, puoi creare regole personalizzate utilizzando espressioni regolari avanzate e caratteri speciali.

Le regular expressions possono sembrare un po' complicate all'inizio, ma una volta che hai compreso i concetti base, potrai sfruttarne appieno il potenziale e semplificare notevolmente il tuo lavoro di programmazione.

## Vedi anche

* Documentazione della libreria Regex di Gleam: https://gleam.run/modules/regex
* Tutorial sulle regular expressions in Gleam: https://gleam.run/tutorials/regex
* Cheatsheet delle regular expressions: https://www.rexegg.com/regex-quickstart.html