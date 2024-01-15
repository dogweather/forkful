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

## Perché
Il convertitore di stringhe in minuscolo è utile quando si lavora con input utente o dati da fonti esterne, in modo da poter uniformare il formato delle stringhe e facilitare le operazioni di confronto e manipolazione dei dati.

## Come Fare
```arduino
String input = "Hello World";
String lowerCase = input.toLowerCase();
Serial.println(lowerCase);
```
L'output di questo codice sarà "hello world", poiché il metodo `toLowerCase()` trasforma tutte le lettere maiuscole in minuscole all'interno della stringa.

## Approfondimento
Il convertitore di stringhe in minuscolo si basa sulle operazioni asciitabellari, che assegnano un numero alle lettere e ai caratteri. In base a questo numero, la funzione `toLowerCase()` sostituisce le lettere maiuscole con la loro corrispondente minuscola. Tieni presente che questo processo può variare leggermente a seconda delle impostazioni di codifica del sistema.

## Vedi Anche
- [Documentazione su toLowerCase()](https://www.arduino.cc/reference/tr/language/variables/data-types/string/functions/tolowercase/)
- [Un esempio di utilizzo di toLowerCase() per elaborare input utente](https://www.arduino.cc/en/Tutorial/StringCaseChanges)