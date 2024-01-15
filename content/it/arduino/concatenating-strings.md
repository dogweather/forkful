---
title:                "Incatenando stringhe"
html_title:           "Arduino: Incatenando stringhe"
simple_title:         "Incatenando stringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione comune in programmazione e può essere utile per un'ampia varietà di progetti. Ad esempio, può essere usata per creare output personalizzati o per combinare dati da diverse fonti.

## Come fare

Per concatenare stringhe in Arduino, è necessario utilizzare la funzione `strcat()`. Vediamo un esempio di codice:

```Arduino
// Dichiarazione delle stringhe
char nome[] = "Mario";
char cognome[] = "Rossi";

// Concatenazione delle due stringhe
strcat(nome, cognome);

// Stampa della stringa concatenata
Serial.println(nome);

// Output: MarioRossi
```

Come puoi vedere, è necessario dichiarare le stringhe che si desidera concatenare e poi utilizzare la funzione `strcat()` per unire le due. Questo è solo un esempio semplice, ma è possibile concatenare più di due stringhe utilizzando lo stesso metodo.

## Approfondimento

La funzione `strcat()` è una delle tante funzioni disponibili per manipolare le stringhe in Arduino. Ci sono anche altre funzioni utili come `strcpy()` per copiare una stringa in un'altra, `strlen()` per ottenere la lunghezza di una stringa e `strtok()` per suddividere una stringa in sottostringhe più piccole.

Inoltre, è importante notare che quando si concatenano stringhe, è necessario prevedere lo spazio sufficiente per contenere la stringa concatenata. In caso contrario, si rischia di sovrascrivere altre variabili o di incorrere in errori di memoria.

## Vedi anche

- [Documentazione di Arduino](https://www.arduino.cc/reference/en/language/functions/string-manipulation/strcat/)
- [Tutorial su stringhe in Arduino](https://www.arduino.cc/en/Tutorial/StringConstructors)
- [Altro esempio di concatenazione stringhe](https://www.webuildinternet.com/2015/08/22/concatenate-string-with-arduino/)