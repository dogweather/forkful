---
title:                "Analisi di HTML"
html_title:           "Bash: Analisi di HTML"
simple_title:         "Analisi di HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Molti siti web sono basati su HTML, quindi la capacità di estrarre informazioni da questo linguaggio di markup può essere estremamente utile per il web scraping, l'analisi dei dati e altre attività informatiche.

## Come Fare

Per iniziare a parsare HTML in Bash, è necessario utilizzare lo strumento "sed" (stream editor) che permette di modificare i contenuti dei file di testo. Vediamo un esempio pratico:

```Bash
#!/bin/bash

# Scarica la pagina web di Google
curl "https://www.google.com" > google.html

# Utilizza "sed" per eliminare tutti i tag HTML e stampare solo il testo
sed "s/<[^>]*>//g" google.html > testo_google.txt

# Stampa il contenuto del file di testo risultante
cat testo_google.txt
```

Output:

```
Google offered in: ខ្មែរ Advertising Programs‎Business Solutions‎Privacy‎About Google‎
```

Come puoi vedere, utilizzando il comando "sed" siamo riusciti a eliminare tutti i tag HTML dalla pagina web di Google e ottenere solo il testo desiderato.

## Deep Dive

Per comprendere meglio il processo di parsing HTML in Bash, è importante capire come funziona lo strumento "sed". Questo è in grado di cercare ed eliminare un determinato pattern all'interno di un file di testo utilizzando le espressioni regolari. Possiamo utilizzare questa funzionalità per cercare i tag HTML e sostituirli con una stringa vuota, ovvero eliminarli.

È importante notare che questa tecnica funziona solo per pagine web con una struttura semplice, come nel caso dell'esempio di Google. Per pagine più complesse, potrebbe essere necessario utilizzare un tool di parsing più avanzato come "grep" o "awk".

## Vedi Anche

- [Documentazione sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Tutorial Regex per Bash](https://linuxhandbook.com/bash-regex/)
- [Esempi pratici di parsing HTML in Bash](https://www.lemoda.net/sh/sed-html/)