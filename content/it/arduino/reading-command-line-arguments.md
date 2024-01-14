---
title:    "Arduino: Lettura degli argomenti dalla riga di comando"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##Perché

Se stai lavorando a un progetto di programmazione con Arduino, potresti voler capire come leggere gli argomenti della riga di comando. Questa conoscenza è particolarmente utile se vuoi creare un programma che possa essere eseguito da un utente che vuole specificare alcuni parametri personalizzati. Continua a leggere per scoprire come farlo!

##Come fare

Per poter leggere gli argomenti della riga di comando in Arduino, devi utilizzare la funzione `Serial.read()`. Di seguito è riportato un esempio di codice che mostra come utilizzare questa funzione per leggere gli argomenti della riga di comando:

```
Arduino Serial.println("Inserisci un parametro:");
while (!Serial.available()){}
parametro = Serial.read();
Serial.print("Hai inserito il parametro ");
Serial.println(parametro);
```

In questo esempio, stiamo chiedendo all'utente di inserire un parametro nella riga di comando. Successivamente, usiamo la funzione `Serial.read()` per leggere il parametro che l'utente ha inserito. Infine, stampiamo il parametro sulla porta seriale per verificare che sia stato letto correttamente.

##Approfondimento

Oltre al metodo sopra descritto, puoi anche utilizzare la funzione `Serial.readString()` per leggere l'intera riga della riga di comando invece di un singolo carattere. Inoltre, puoi utilizzare il comando `Serial.parseInt()` per convertire il parametro letto in un valore intero invece di una stringa.

##Vedi anche

- Tutorial sull'utilizzo di `Serial.read()`: https://www.arduino.cc/reference/en/language/functions/communication/serial/read/
- Guida su come leggere argomenti da riga di comando in Arduino: https://www.arduino.cc/en/Tutorial/CommandLineArguments
- Esempio di progetto Arduino che legge argomenti dalla riga di comando: https://create.arduino.cc/projecthub/MakerRobotics/how-to-pass-commandline-arguments-to-an-arduino-sketch-dc663b