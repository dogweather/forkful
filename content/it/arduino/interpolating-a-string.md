---
title:                "Interpolazione di una stringa"
html_title:           "Arduino: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Interpolare una stringa significa sostituire parti di una frase con valori dinamici. Questo è utile per creare stringhe personalizzate basate su dati in tempo reale, come ad esempio la temperatura o il valore di un sensore. I programmatori utilizzano l'interpolazione di stringhe per rendere i loro codici più flessibili e dinamici.

## Come fare:
Ecco un esempio di come interpolare una stringa utilizzando la sintassi della lingua C++ in Arduino:

```
int numero = 5; // dichiara una variabile intera
String stringa = "Il numero interpolato è: ${numero}"; // dichiara una stringa con il valore dinamico ${numero}

Serial.println(stringa); // stamperà "Il numero interpolato è: 5" sulla console seriale
```

## Approfondimento:
L'interpolazione di stringhe è una tecnica utilizzata da molti linguaggi di programmazione, incluso C++. In passato, i programmatori dovevano costruire manualmente le stringhe unendo pezzi di testo e variabili. Con l'avvento dell'interpolazione di stringhe, questo processo è diventato più facile e leggibile.

In Arduino, è possibile utilizzare anche la funzione ```sprintf()``` per interpolare le stringhe. Tuttavia, questa funzione richiede più spazio di memoria e non permette di utilizzare variabili dinamiche come ```String```.

## Vedi anche:
- [Documentazione di Arduino su String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Documentazione di C++ sull'interpolazione di stringhe](https://www.cplusplus.com/reference/cstdio/sprintf/)