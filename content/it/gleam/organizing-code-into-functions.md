---
title:                "Organizzazione del codice in funzioni"
date:                  2024-01-26T01:10:06.436856-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizzazione del codice in funzioni"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Organizzare il codice in funzioni significa suddividere il comportamento di un programma in parti più piccole e riutilizzabili. I programmatori fanno ciò per rendere il codice più chiaro, più manutenibile e per evitare ripetizioni.

## Come fare:
Ecco un semplice esempio di organizzazione del codice in funzioni in Gleam:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let somma = add(3, 4)
  somma
}

// Esempio di output
// 7
```

In questo frammento, `add` è una funzione che prende due valori e li somma. `main` è dove chiamiamo `add` e gestiamo il risultato.

## Approfondimento
Storicamente, il concetto di funzioni (o 'sottoprogrammi') ha rivoluzionato la programmazione, spianando la strada alla programmazione strutturata negli anni '60 e oltre. Le funzioni incoraggiano un approccio modulare, dove i problemi sono divisi in sottoproblemi, risolti indipendentemente e composti per risolvere la questione più ampia.

In Gleam, che è un linguaggio a tipizzazione forte, le funzioni includono anche informazioni sul tipo, assicurando che il loro uso sia coerente con la loro definizione. Questo riduce gli errori e chiarisce le intenzioni.

Le alternative alle funzioni includono la codifica in linea, dove la logica è scritta ripetutamente. Sebbene talvolta sia più veloce per piccoli compiti una tantum, la codifica in linea non scala bene per applicazioni più grandi.

I dettagli implementativi da considerare quando si organizza in funzioni possono includere la composizione delle funzioni, dove le funzioni sono usate come blocchi costruttivi, e le funzioni di ordine superiore, che prendono altre funzioni come argomenti o le restituiscono, aggiungendo flessibilità a come il codice è organizzato ed eseguito.

## Vedi Anche
Per maggiori informazioni sulle funzioni in Gleam, puoi esplorare la documentazione ufficiale a:
- [Funzioni del linguaggio Gleam](https://gleam.run/book/tour/functions.html)

Oppure esplorare concetti di programmazione più ampi:
- [Rete per Sviluppatori Mozilla sulle Funzioni JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions)
- [Impara un po' di Erlang per la Grande Bontà! - Su Moduli e Funzioni](https://learnyousomeerlang.com/modules)