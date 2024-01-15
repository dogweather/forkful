---
title:                "Utilizzare le espressioni regolari"
html_title:           "Gleam: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché utilizzare espressioni regolari in Gleam

Se stai scrivendo codice in Gleam, è probabile che presto o tardi ti troverai di fronte alla necessità di manipolare stringhe di testo. Le espressioni regolari sono uno strumento potente per trovare e manipolare determinati pattern all'interno di stringhe di testo. Ti permettono di risparmiare tempo e sforzi nella manipolazione di stringhe e possono aiutarti a scrivere codice più efficiente e accurato.

## Come utilizzare espressioni regolari in Gleam

Per utilizzare le espressioni regolari, il primo passo è importare il modulo `regex` di Gleam. Puoi farlo utilizzando il seguente codice:

```Gleam
import regex

// Resto del codice
```

Una volta importato il modulo, puoi utilizzare la funzione `Regex.replace` per cercare e sostituire un determinato pattern all'interno di una stringa. Ad esempio:

```Gleam
let input = "Hello, World!"
let output = Regex.replace(input, Regex.compile("[Ww]orld"), "new_world")

// output diventerà "Hello, new_world!"
```
Puoi anche utilizzare la funzione `Regex.match` per cercare un determinato pattern all'interno di una stringa e ottenere le corrispondenze trovate. Ad esempio:

```Gleam
let input = "01/01/2021"
let matches = Regex.match(input, Regex.compile("([0-9]{2})/([0-9]{2})/([0-9]{4})"))

// matches diventerà Just(["01/01/2021", "01", "01", "2021"])
```

## Approfondimenti sull'utilizzo di espressioni regolari in Gleam

Una volta compreso come utilizzare le espressioni regolari in Gleam, puoi esplorare ulteriormente le diverse funzioni e opzioni disponibili nel modulo `regex`. Puoi anche imparare a creare espressioni regolari più complesse, incluse le cosiddette "capture groups" che consentono di ottenere specifiche porzioni di una stringa corrispondente al pattern cercato.

Oltre a ciò, è importante anche comprendere le "best practices" nell'utilizzo delle espressioni regolari, come ad esempio la corretta gestione delle eccezioni e la verifica della correttezza dei pattern utilizzati.

## Vedi anche

- Documentazione del modulo `regex` di Gleam: https://gleam.run/libraries/regex/
- Guida alle espressioni regolari: https://regexone.com/
- Tutorial su come utilizzare espressioni regolari in Gleam: https://serokell.io/blog/gleam-regex-tutorial