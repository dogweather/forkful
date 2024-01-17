---
title:                "Generare numeri casuali"
html_title:           "Arduino: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

- # Cosa & Perché? 
Generare numeri casuali è un'operazione utile per molti programmatori, in quanto consente di creare un flusso di dati che non rispetta uno schema predeterminato. Ciò può essere utilizzato per creare sorprese o varietà all'interno del programma.

- # Come fare:
Una funzione molto conveniente per generare numeri casuali è random (), che restituisce un numero compreso tra 0 e 255. Vediamo un esempio:
```arduino 
int randomNumber = random(0,10);
Serial.println(randomNumber);
```
Questo codice genererà un numero casuale tra 0 e 10 e lo stampa sulla console seriale.

- # Approfondimento: 
Generare numeri casuali è stato uno degli usi principali dei computer nei primi tempi della programmazione. Oggi ci sono anche altri modi per generarli, come tramite sensori o algoritmi matematici più complessi. Inoltre, è importante notare che i numeri "casuali" ottenuti dai computer sono in realtà pseudo-casuali, poiché sono basati su algoritmi deterministici.

- # Vedi anche: 
- Documentazione ufficiale su random () - https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Esempi di progetti che utilizzano numeri casuali - https://create.arduino.cc/projecthub/projects/tags/random