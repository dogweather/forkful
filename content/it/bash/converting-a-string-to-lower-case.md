---
title:                "Trasformare una stringa in minuscolo"
html_title:           "Bash: Trasformare una stringa in minuscolo"
simple_title:         "Trasformare una stringa in minuscolo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Convertire una stringa in minuscolo significa rendere tutti i caratteri della stringa con lettere minuscole. I programmatori spesso lo fanno per uniformare i dati all'interno di un programma e facilitare la ricerca e il confronto tra le stringhe.

## Come fare:
Per convertire una stringa in minuscolo in Bash, possiamo utilizzare il comando `tr` combinato con l'opzione `[:upper:]` per selezionare tutti i caratteri maiuscoli e l'opzione `[:lower:]` per convertirli in minuscolo. Esempio:

```Bash
stringa="Ciao Mondo!"
echo "$stringa" | tr '[:upper:]' '[:lower:]'
```

L'output sarà `ciao mondo!`.

## Approfondimento:
Questa funzionalità è stata introdotta nella versione 2.0 di Bash ed è disponibile anche in altri linguaggi di programmazione come Python e Java. Un'alternativa per convertire una stringa in minuscolo in Bash è utilizzare il comando `sed`, ma questo richiede una sintassi leggermente diversa. 

Per implementare la conversione in minuscolo, il sistema operativo utilizza il codice ASCII per identificare e modificare i caratteri. Ciò significa che, oltre alle lettere dell'alfabeto, anche i caratteri speciali e i numeri possono essere convertiti in minuscolo.

## Vedi anche:
- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Conversione stringa in minuscolo in Python](https://www.programiz.com/python-programming/methods/string/lower)
- [Conversione stringa in minuscolo in Java](https://www.tutorialspoint.com/java/lang/string_tolowercase.htm)