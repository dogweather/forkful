---
title:    "Gleam: Estrazione di sottostringhe"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Cosa spinge ad estrarre sottostringhe? Uno dei motivi principali per farlo è per ottenere una parte specifica di una stringa più grande. Ad esempio, potresti voler ottenere solo il cognome da un nome completo o solo il dominio da un indirizzo email. L'estrazione di sottostringhe è un'operazione comune e utile nel processo di manipolazione delle stringhe.

## Come Fare

Per estrarre una sottostringa in Gleam, utilizzeremo la funzione `substring` della libreria `gleam/strings`.

```Gleam
import gleam/strings

// Definiamo una stringa di esempio.
let parola = "ciao mondo"

// Utilizziamo la funzione `substring` per estrarre la sottostringa con indice di inizio 0 e lunghezza 4.
let sottostringa = strings.substring(parola, 0, 4)

// Output: "ciao"
```

Un altro modo per estrarre una sottostringa è utilizzare l'operatore `..`, che rappresenta un intervallo di indici.

```Gleam
import gleam/strings

// Definiamo una stringa di esempio.
let parola = "ciao mondo"

// Utilizziamo l'operatore `..` per estrarre la sottostringa con indici da 5 a 8 (estremi inclusi).
let sottostringa = strings.substring_range(parola, 5 .. 8)

// Output: "mon"
```

Entrambi gli esempi hanno prodotto la stessa sottostringa, ma utilizzano metodi diversi. È importante ricordare che l'indice di inizio del primo carattere in Gleam è 0.

## Approfondimento

La funzione `substring` accetta anche valori negativi per l'indice di inizio e l'indice di fine. Se si passa un valore negativo come indice di inizio, conta dalla fine della stringa anziché dall'inizio.

```Gleam
let parola = "ciao mondo"

// Utilizziamo `-4` per estrarre gli ultimi 4 caratteri della stringa.
let sottostringa = strings.substring(parola, -4, 0)

// Output: "ondo"
```

Ci sono anche altri metodi nella libreria `gleam/strings` che sono utili per l'estrazione delle sottostringhe, come ad esempio`substring_except`.

## Vedi Anche

- Documentazione ufficiale della libreria `gleam/strings`: https://gleam.run/documentation/gleam_stdlib/strings.html#substring
- Esempi di utilizzo di Gleam: https://github.com/gleam-lang/gleam/tree/master/examples
- Tutorial di Gleam: https://gleam.run/book/tour.html