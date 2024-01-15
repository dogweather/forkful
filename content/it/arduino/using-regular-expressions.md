---
title:                "Utilizzare le espressioni regolari"
html_title:           "Arduino: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

Arduino permette di utilizzare espressioni regolari per manipolare e analizzare dati in modo più efficiente. I regex sono uno strumento potente per il controllo dei dati e la ricerca di pattern specifici all'interno di una stringa.

## Perché

Se stai lavorando con grandi quantità di dati o hai l'esigenza di estrarre informazioni specifiche da una stringa di testo, le espressioni regolari possono semplificare notevolmente il tuo lavoro. Inoltre, utilizzando regolarmente i regex puoi migliorare le tue abilità di programmazione e diventare più efficiente nella manipolazione dei dati.

## Come Utilizzare le Espressioni Regolari su Arduino

Per utilizzare le espressioni regolari su Arduino, devi includere la libreria "Regex" nel tuo sketch. Puoi farlo iniziando il tuo codice con il seguente comando: 

```Arduino
#include <Regex.h>
```

Una volta incluso il file della libreria, puoi definire un'istanza della classe Regex nel tuo codice e utilizzarla per manipolare le stringhe di testo. Ad esempio, puoi utilizzare la funzione `match()` per cercare un pattern specifico all'interno di una stringa e ottenere le corrispondenze. 

Ecco un esempio di codice che cerca la parola "Arduino" all'interno di una stringa e stampa il risultato sulla console seriale:

```Arduino
#include <Regex.h>

Regex regex; // Definizione dell'istanza della libreria

void setup() {
  Serial.begin(9600); // Inizializzazione della comunicazione seriale
  regex.match("This is an Arduino tutorial", "Arduino"); // Ricerca del pattern nella stringa
}

void loop() {
  // Se il pattern è stato trovato, viene stampato sulla console seriale
  if(regex.matched()) {
    Serial.println(regex.endMatch()); // Stampa il risultato della ricerca
  }
}
```

L'output del codice sarà: "Arduino".

## Approfondimenti sui Regex

Se vuoi approfondire le tue conoscenze sui regex e scoprire tutte le possibili funzioni e sintassi disponibili, puoi consultare la documentazione ufficiale della libreria Regex per Arduino. Inoltre, ci sono numerosi tutorial e guide disponibili online che possono aiutarti a utilizzare in modo più avanzato le espressioni regolari su Arduino.

## Vedi Anche

- [Documentazione ufficiale della libreria Regex per Arduino](https://github.com/arduino-libraries/Regex)
- [Tutorial su come utilizzare le espressioni regolari su Arduino](https://www.arduino.cc/en/Tutorial/RegexpFunction) 
- [Guida completa sull'utilizzo dei regex su Arduino](https://randomnerdtutorials.com/arduino-regex-tutorial-and-examples/)