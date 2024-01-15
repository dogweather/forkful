---
title:                "Estrazione di sottostringhe"
html_title:           "Arduino: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se stai programmando con Arduino, potresti trovarti nella situazione in cui hai una stringa di testo più lunga e vuoi estrarne solo una parte specifica. Ecco dove entra in gioco l'estrazione di sottostringhe. Può sembrare un'operazione insignificante, ma può essere molto utile per semplificare il tuo codice e renderlo più efficiente.

## Come fare

Per estrarre una sottostringa da una stringa di testo su Arduino, puoi utilizzare la funzione `substring()` incorporata. Dovrai specificare il punto di partenza e la lunghezza della sottostringa che vuoi estrarre.

```
ArduinoString stringa = "Hello World";
ArduinoString sottostringa = stringa.substring(6, 5);

Serial.println(sottostringa); // Output: World
```

In questo esempio, abbiamo assegnato la stringa "Hello World" alla variabile `stringa` e abbiamo utilizzato la funzione `substring()` per estrarre la sottostringa "World" a partire dalla sesta posizione (contando i caratteri dalla posizione 0). Il secondo parametro della funzione indica la lunghezza della sottostringa, quindi nel nostro caso è di 5 caratteri, partendo dalla sesta posizione.

Puoi anche utilizzare la funzione `substring()` per estrarre una sottostringa dalla fine della stringa, specificando un valore negativo per il primo parametro. Ad esempio, per estrarre l'ultima parola della stringa "Hello World", puoi farlo in questo modo:

```
ArduinoString stringa = "Hello World";
ArduinoString ultima_parola = stringa.substring(-5);

Serial.println(ultima_parola); // Output: World
```

## Approfondimento

Ci sono alcune cose importanti da tenere a mente quando si estraggono sottostringhe su Arduino. Innanzitutto, la funzione `substring()` restituirà una nuova ArduinoString, quindi è importante assegnarla a una variabile se vuoi utilizzarla in seguito. Inoltre, se specifici un punto di partenza al di fuori dell'intervallo della stringa di origine, si verificherà un errore.

Inoltre, la funzione `substring()` non modificherà la stringa di origine, quindi se vuoi eliminare la parte di stringa che hai estratto, dovrai utilizzare la funzione `remove()`.

```
ArduinoString stringa = "Hello World";
stringa.remove(6, 5); // Rimuove la sottostringa "World"

Serial.println(stringa); // Output: Hello
```

Inoltre, puoi utilizzare anche altre funzioni di ArduinoString per manipolare le sottostringhe, come `concat()` per unire più sottostringhe o `replace()` per sostituire una sottostringa con un'altra.

## Vedi anche

- [Documentazione ufficiale di ArduinoString](https://www.arduino.cc/reference/it/language/variables/utilities/arduinostring/)
- [Tutorial su come utilizzare le stringhe su Arduino](https://www.circuitbasics.com/arduino-strings/)
- [Tutorial video su come lavorare con le sottostringhe su Arduino](https://www.youtube.com/watch?v=oIuOSe0jODO)