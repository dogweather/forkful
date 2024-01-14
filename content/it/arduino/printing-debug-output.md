---
title:    "Arduino: Stampa dell'output di debug"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Spesso quando si programma su Arduino, può essere utile visualizzare informazioni di debug durante l'esecuzione del codice. Ciò consente di avere un maggiore controllo sul funzionamento del programma e di individuare eventuali errori.

## Come fare

Per stampare output di debug su Arduino, è possibile utilizzare la funzione "Serial.println()". Questa funzione invia una stringa di testo al monitor seriale, che può essere visualizzata utilizzando il software di monitoraggio seriale come il monitor seriale dell'IDE di Arduino o un programma come PuTTY.

```Arduino
void setup() {
  // Inizializza la comunicazione seriale a una velocità di 9600 baud
  Serial.begin(9600); 
}

void loop() {
  // Stampa la stringa "Hello World!" nel monitor seriale
  Serial.println("Hello World!"); 
  delay(1000); // Aspetta un secondo
}
```
### Esempio di output:
```
Hello World!
Hello World!
Hello World!
```

In più, il monitor seriale può essere utilizzato per inviare valori numerici o variabili come parte della stringa di testo.

```Arduino
int temperature = 25;
float humidity = 65.5;

void setup() {
  Serial.begin(9600);
}

void loop() {
  // Stampa la temperatura e l'umidità attuali nel monitor seriale
  Serial.println("La temperatura è: " + String(temperature) + "°C");
  Serial.println("L'umidità è: " + String(humidity) + "%");
  delay(1000);
}
```
### Esempio di output:
```
La temperatura è: 25°C
L'umidità è: 65.5%
```

Inoltre, è possibile utilizzare la funzione "Serial.print()" per stampare senza una nuova riga alla fine di ogni chiamata, utile per la formattazione di output più complesse.

## Approfondimento

Una delle funzioni più utili del monitor seriale è la possibilità di visualizzare i valori delle variabili durante l'esecuzione del programma. Questo può aiutare a individuare problemi o a verificare se le variabili stanno assumendo i valori corretti.

Oltre alle funzioni "Serial.print()" e "Serial.println()", ci sono anche altre funzioni utili per il debug, come "Serial.write()" che invia byte di dati al monitor seriale e "Serial.read()" che legge byte del monitor seriale.

Inoltre, il monitor seriale può essere impostato per visualizzare diverse informazioni, come timestamp delle chiamate delle funzioni o l'utilizzo della memoria, utilizzando le impostazioni disponibili nell'IDE di Arduino.

## Vedi anche

- [Documentazione ufficiale di Arduino sulla comunicazione seriale](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial di Adafruit sul monitor seriale di Arduino](https://learn.adafruit.com/arduino-tips-tricks-and-techniques/serial-console-output)
- [Guida di SparkFun su come leggere e scrivere sul monitor seriale di Arduino](https://learn.sparkfun.com/tutorials/terminal-basics/arduino-serial-monitor)