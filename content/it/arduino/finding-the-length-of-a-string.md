---
title:    "Arduino: Trova la lunghezza di una stringa."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Quando si lavora con Arduino, a volte può essere necessario conoscere la lunghezza di una stringa di caratteri. Questa informazione è utile in molte applicazioni, come ad esempio la manipolazione dei dati o la creazione di condizioni per eseguire determinate azioni. In questo post, vi mostrerò come trovare la lunghezza di una stringa utilizzando Arduino.

## Come Fare
Per prima cosa, assicurati di avere un'idea generale su come funziona Arduino e di aver installato l'IDE di Arduino sul tuo computer. Una volta fatto ciò, puoi seguire i seguenti passaggi per trovare la lunghezza di una stringa:

1. Dichiarare la stringa che si desidera analizzare: ```Arduino
char stringa[] = "Esempio";
```
2. Utilizzare la funzione ```strlen()``` per trovare la lunghezza della stringa: ```Arduino
int lunghezza = strlen(stringa);
```
3. Stampare il risultato: ```Arduino
Serial.println(lunghezza);
```

Una volta caricato il codice sull'Arduino, il risultato dovrebbe apparire nel monitor seriale come "7", poiché la stringa "Esempio" contiene sette caratteri.

## Approfondimento
La funzione ```strlen()``` è inclusa nella libreria standard ```string.h``` di Arduino. Essa restituisce il numero di caratteri all'interno di una stringa, escluso il carattere di terminazione null.

È importante notare che la funzione ```strlen()``` conta solo i caratteri alfanumerici, quindi i simboli speciali come spazi, punti e virgole non vengono considerati nella lunghezza della stringa.

Inoltre, se si utilizzano stringhe con valori numerici, la funzione ```strlen()``` non considera le cifre ma solo il numero di caratteri. Ad esempio, se si dichiara la stringa "2456", la lunghezza restituita sarà 4, anche se la stringa rappresenta in realtà un numero composto da quattro cifre.

## Vedi Anche
- Tutorial su come utilizzare la funzione ```strlcp()``: http://bit.ly/2I0W1lP
- Ulteriori informazioni sulle stringhe in Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Esempi di progetti Arduino che utilizzano stringhe: http://bit.ly/2IjJ5KU

Grazie per aver letto questo post. Spero che ora tu abbia una migliore comprensione di come trovare la lunghezza di una stringa utilizzando Arduino. Buona programmazione!