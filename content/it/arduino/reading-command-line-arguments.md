---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Arduino: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché 

Ci sono molte situazioni in cui è indispensabile per un programmatore leggere e comprendere gli argomenti della riga di comando. Questo articolo ti spiegherà come farlo utilizzando la programmazione di Arduino.

## Come Fare

Per leggere gli argomenti della riga di comando in Arduino, è necessario utilizzare la funzione "Serial.readString()". Questa funzione legge e restituisce l'argomento come una stringa, che può essere successivamente elaborata dal programma. Ecco un esempio di codice che legge l'argomento e lo stampa sulla console seriale:

```
Arduino.setup() {
  Serial.begin(9600); //inizializza la comunicazione seriale
}

void loop() {
  if (Serial.available()) { //controlla se sono disponibili dati sulla porta seriale
    String arg = Serial.readString(); //legge l'argomento e lo salva nella variabile "arg"
    Serial.println(arg); //stampa l'argomento sulla console seriale
  }
}
```

Se si desidera utilizzare l'argomento per eseguire un'operazione specifica, è possibile utilizzare la funzione "analogWrite()" di Arduino. Ad esempio, se vogliamo controllare la luminosità di un LED utilizzando l'argomento della riga di comando, possiamo fare così:

```
Arduino.setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    String arg = Serial.readString();
    int brightness = arg.toInt(); //converte la stringa in un valore numerico intero
    analogWrite(LED_PIN, brightness); //imposta la luminosità del LED sul valore letto dalla riga di comando
  }
}
```

## Approfondimento

La funzione "Serial.readString()" legge solo il primo argomento della riga di comando. Se ci sono più argomenti separati da spazi, è possibile utilizzare la funzione "Serial.parseInt()" per leggerli uno per uno. Ad esempio, se abbiamo una riga di comando del tipo "led 255 100" dove il primo argomento è il nome dell'operazione da eseguire e gli altri due sono valori numerici, possiamo utilizzare questo codice per estrarre tutti e tre gli argomenti:

```
Arduino.setup() {
  Serial.begin(9600);
}

void loop() {
  if (Serial.available()) {
    String operation = Serial.readString(); //primo argomento: "led"
    int value1 = Serial.parseInt(); //secondo argomento: 255
    int value2 = Serial.parseInt(); //terzo argomento: 100
    //qui è possibile eseguire un'operazione specifica in base al nome dell'operazione e ai valori numerici
  }
}
```

## Vedi Anche

- [Reference della funzione Serial.readString()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
- [Tutorial sul controllo dei LED con Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Fade)