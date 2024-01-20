---
title:                "Utilizzare le espressioni regolari"
html_title:           "Fish Shell: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 

Usare le espressioni regolari (regex) è una pratica comune tra i programmatori per gestire e manipolare dati di testo. Questi sono modelli di ricerca che consentono di trovare e sostituire testo in modo efficiente in un file o in una stringa di testo. I regex possono essere utilizzati in molti linguaggi di programmazione, incluso Fish Shell.

## Come fare:

Utilizzare le espressioni regolari in Fish Shell è semplice e molto simile ad altri linguaggi di programmazione. Di seguito sono riportati alcuni esempi di codice e il loro output utilizzando le espressioni regolari in Fish Shell.

```
Fish Shell> string='La mia password è p@ssw0rd'
Fish Shell> echo $string | grep -o '\S*@'     
p@ssw0rd
```

In questo esempio, utilizziamo `grep` per estrarre la nostra password dalla stringa utilizzando l'espressione regolare `'\S*@'`, che significa "trova qualsiasi sequenza di caratteri non vuoti seguita da una @". Il risultato che otteniamo è la nostra password, risolta correttamente.

```
Fish Shell> string='Maria ha 30 anni, Giovanni ne ha 25.'
Fish Shell> echo $string | grep -o '\d\d'
30
25
```

In questo secondo esempio, utilizziamo nuovamente `grep` per trovare tutti i numeri all'interno della stringa, utilizzando il comando `'\d\d'`, che significa "trova qualsiasi sequenza di due numeri consecutivi". Il risultato è l'elenco dei numeri trovati nella stringa.

## Approfondimento:

Le espressioni regolari sono state originariamente sviluppate negli anni '50 e sono state successivamente incorporate in molti linguaggi di programmazione. Alcuni altri strumenti utilizzati per il matching di pattern nei testi sono AWK e SED.

Inoltre, è importante notare che l'uso delle espressioni regolari può differire leggermente tra i vari linguaggi di programmazione. Ad esempio, in Fish Shell, è possibile utilizzare `egrep` invece di `grep`, per ottenere una versione estesa di `grep` che supporta sintassi delle espressioni regolari aggiuntive.

Per approfondire l'uso delle espressioni regolari in Fish Shell, è possibile consultare la documentazione ufficiale sul sito [Fish Shell](https://fishshell.com/docs/current/guides.html).

## Vedi anche:

- [Utilizzare AWK per operazioni con i dati di testo](https://www.gnu.org/software/gawk/manual/gawk.html#AWK-Introduction)