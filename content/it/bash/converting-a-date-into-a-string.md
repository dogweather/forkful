---
title:                "Convertire una data in una stringa"
html_title:           "Bash: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è utile quando si vuole rappresentare la data in un formato specifico o quando si vuole utilizzare la data come parte di una stringa più grande.

## Come fare

Per convertire una data in una stringa, è possibile utilizzare il comando ```date```. Ad esempio, per ottenere la data corrente nel formato "dd/mm/yyyy", si può utilizzare il seguente codice:

```Bash
date +%d/%m/%Y
```

L'output sarà qualcosa del genere:

```Bash
30/07/2020
```

In questo esempio, il carattere "%" viene utilizzato per specificare il formato della data. E' possibile utilizzare diversi caratteri per ottenere formati diversi. Ad esempio, "%m" rappresenta il mese, "%Y" rappresenta l'anno e così via.

## Approfondimento

Il comando ```date``` offre molte opzioni per personalizzare il formato della data e l'output. E' possibile consultare la documentazione ufficiale per maggiori dettagli. Inoltre, è possibile anche utilizzare il comando ```man date``` per avere una panoramica delle opzioni disponibili e dei formati supportati.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/html_node/)
- [Guida rapida al comando date](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Manuale di Bash](https://www.linux.org/docs/man1/bash.html)