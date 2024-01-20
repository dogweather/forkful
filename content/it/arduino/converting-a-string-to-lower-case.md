---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabetici maiuscoli in minuscoli. Questo trucco è utilizzato dai programmatori quando desiderano confrontare due stringhe in modo insensibile al caso o uniformare l'input dell'utente.

## Come si fa:

Ecco come farlo con Arduino. Il seguente codice cambia tutti i caratteri della stringa a minuscolo.

```Arduino
String str = "CIAO MONDO!";
str.toLowerCase();
```
Dopo aver eseguito il codice sopra, otterremo il seguente output.

```Arduino
"!="ciao mondo!"
```
## Approfondimento

Storicamente, la conversione di stringhe in minuscolo è stata utilizzata per semplificare le operazioni informatiche. Ad esempio, le ricerche di parole in un testo sono semplificate se tutto è nello stesso caso.

Ci sono alternative alla funzione `toLowerCase()` di Arduino per convertire una stringa in minuscolo. Alcuni programmatori cambiano manualmente ogni carattero della stringa utilizzando la funzione `tolower()`. Ma in Arduino, `toLowerCase()` è l'approccio più efficiente.

`toLowerCase()` internamente itera attraverso ciascun carattere della stringa e controlla se è un carattere maiuscolo. Se lo è, lo converte in minuscolo. Importante sapere: stringhe Arduino sono immutabili, quindi `toLowerCase()` in realtà crea una nuova stringa. 

## Vedere Anche

Per approfondire, consulta:
- [Documentazione ufficiale della funzione `toLowerCase()` di Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Un post sul Forum di Arduino che discute di vari metodi per convertire stringhe](https://forum.arduino.cc/index.php?topic=396450.0)