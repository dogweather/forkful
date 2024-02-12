---
title:                "Conversione di una stringa in minuscolo"
aliases: - /it/arduino/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:47.354267-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabetici in lettere minuscole. Lo si fa per uniformare i dati, semplificare il confronto tra stringhe ed evitare errori di case sensitivity.

## How to:
Per convertire una stringa in minuscolo su Arduino, utilizza il metodo `toLowerCase()`. Ecco un esempio:

```arduino
String myString = "Ciao Mondo!";
myString.toLowerCase();
Serial.begin(9600);
Serial.println(myString); // Stampará: ciao mondo!
```

Output:
```
ciao mondo!
```

## Deep Dive
Nella storia della programmazione, la necessità di convertire testo tra maiuscolo e minuscolo è sempre stata presente. Arduino fornisce un approccio semplice e diretto con il metodo `toLowerCase()`, che fa parte della classe `String`.

Alternativamente, potresti usare i metodi classici del C, come `tolower()` applicato a ciascun carattere di un array `char`. Tuttavia, questo richiede una comprensione più profonda dei puntatori e della gestione della memoria.

Il metodo `toLowerCase()` di Arduino scorre ogni carattere della stringa e, se il carattere è una lettera maiuscola, la converte nella sua corrispondente minuscola, secondo la codifica ASCII. Questo processo è insensibile alla localizzazione, quindi è valido solo per i caratteri A-Z.

## See Also
- [Arduino Reference: String toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Wikipedia: ASCII](https://it.wikipedia.org/wiki/ASCII)
- [ASCII Table and Description](https://www.asciitable.com/)
