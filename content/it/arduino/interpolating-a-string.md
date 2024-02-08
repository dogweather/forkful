---
title:                "Interpolazione di una stringa"
aliases:
- it/arduino/interpolating-a-string.md
date:                  2024-01-20T17:50:21.167265-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
L'interpolazione di stringhe consente di inserire variabili dentro una stringa di testo. Lo facciamo per rendere il codice più leggibile e per comporre messaggi dinamici senza complicarsi la vita.

## How to: (Come fare:)
In Arduino, l'interpolazione diretta di stringhe non è supportata come in altri linguaggi, ma possiamo raggiungere lo stesso risultato con `sprintf()` o concatenando stringhe.

```Arduino
char buffer[50]; 
int temperatura = 23;

void setup() {
  Serial.begin(9600);
  
  // Utilizzo di sprintf() per interpolare la variabile temperatura nella stringa
  sprintf(buffer, "La temperatura e' %d gradi Celsius.", temperatura);
  Serial.println(buffer);
}

void loop() {
  // Il loop viene lasciato vuoto in questo esempio
}
```

Output:
```
La temperatura e' 23 gradi Celsius.
```

```Arduino
String messaggio; 
int umidita = 60;

void setup() {
  Serial.begin(9600);
  
  // Concatenazione di stringhe per ottenere un risultato simile all'interpolazione
  messaggio = "Il livello di umidita' e' " + String(umidita) + "%.";
  Serial.println(messaggio);
}

void loop() {
  // Anche qui, il loop è vuoto.
}
```

Output:
```
Il livello di umidita' e' 60%.
```

## Deep Dive (Approfondimento)
L'Arduino non ha un supporto nativo per l'interpolazione di stringhe come Python o JavaScript. Storicamente, si è sempre fatto affidamento su funzioni come `sprintf()` o su operazioni di concatenazione. La funzione `sprintf()` è potente, ma va usata con cautela per non superare la dimensione dell'array buffer. La concatenazione con la classe `String` è più semplice, ma può portare a frammentazione della memoria, soprattutto con Arduino che ha risorse limitate.

Alternativamente, si può costruire una funzione custom per replicare l'interpolazione di stringhe, ma questo aumenta la complessità del codice.

In ogni caso, sia che si utilizzi `sprintf()` che la concatenazione con la classe `String`, ricordati di gestire correttamente la memoria per evitare errori a tempo di esecuzione.

## See Also (Vedi anche)
Per approfondire, consulta la documentazione ufficiale e altri esempi pratici:
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Using C/C++ Standard Library for AVR (Memory Considerations)](https://www.nongnu.org/avr-libc/)
