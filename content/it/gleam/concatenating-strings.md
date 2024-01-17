---
title:                "Concatenazione di stringhe"
html_title:           "Gleam: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La concatenazione di stringhe è il processo di unire più stringhe in una nuova stringa più lunga. I programmatori spesso fanno questo per costruire output leggibili e formattati o per creare stringhe dinamiche basate su input o variabili.

## Come:

```Gleam
let nome = "Maria"
let cognome = "Rossi"
let saluto = "Ciao, " <> nome <> " " <> cognome
```

Questo codice combina tre stringhe in una nuova stringa, creando l'output "Ciao, Maria Rossi". 

## Approfondimento:

La concatenazione di stringhe è un'operazione comune in molti linguaggi di programmazione e ha radici nell'informatica fin dagli anni '50. Alcuni linguaggi utilizzano il simbolo "+" per la concatenazione, mentre altri come Gleam utilizzano "<>" come operatore.

In alcuni casi, concatenare stringhe può essere meno efficiente rispetto all'uso di altri metodi per gestire output e formattazione. È importante valutare le alternative e utilizzare la concatenazione solo quando necessario.

## Vedi anche:

Per saperne di più sulla concatenazione di stringhe in Gleam, puoi consultare la documentazione ufficiale su [Gleam's strings module](https://gleam.run/modules/random) e [Gleam's operators](https://gleam.run/cheatsheet#operators).