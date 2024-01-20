---
title:                "Capitalizzare una stringa"
html_title:           "Arduino: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---

## Che cosa & Perché?
Capitalizzare una stringa significa convertire tutte le lettere in una stringa in maiuscolo. I programmatori lo fanno per aumentare la leggibilità e standardized i dati.

## Come fare:
Ecco un esempio di codice semplice in Arduino per capitalizzare una stringa:

```Arduino 
char str[12] = "Ciao Mondo";
for(int i = 0; str[i]; i++)
    str[i] = toupper(str[i]);
```
Output di questo codice sarà:
```Arduino 
CIAO MONDO
```
Qui, abbiamo utilizzato la funzione `toupper()` della libreria `ctype.h` per convertire ogni lettera in maiuscolo.

## Approfondimento
La necessità di capitalizzare le stringhe risale ai primi giorni della programmazione, quando la distinzione tra maiuscole e minuscole non era sempre chiara e potrebbe causare problemi nel codice. 

In alternativa alla funzione `toupper()`, potresti utilizzare la funzione `toUpperCase()` di Arduino. Questa funzione converte tutta la stringa in maiuscole al posto di convertire ogni singolo carattere come fa la funzione `toupper()`.

Ecco un esempio di come utilizzare `toUpperCase()`:

```Arduino 
String str = "Ciao Mondo";
str.toUpperCase();
```

E il suo output sarebbe:
```Arduino 
CIAO MONDO
```
In termini di dettagli dell'implementazione, sappi che entrambe le funzioni funzionano solo con stringhe ASCII standard.

## Vedi anche
- Documentazione ufficiale di Arduino su [toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- Discorso sulla standardizzazione delle stringhe in [Wikipedia](https://en.wikipedia.org/wiki/String_interning)
- Metodo alternativo per capitalizzare le stringhe usando [toupper()](https://www.cplusplus.com/reference/cctype/toupper/).