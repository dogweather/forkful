---
title:    "Arduino: Lettura di argomenti della riga di comando"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei un appassionato di tecnologia e programmazione, probabilmente hai sentito parlare del microcontrollore Arduino. Questo piccolo dispositivo è ampiamente utilizzato per creare progetti di elettronica e robotica. Una delle funzionalità più interessanti di Arduino è la possibilità di leggere gli argomenti dalla riga di comando. Ma perché dovresti leggere gli argomenti dalla riga di comando? Continua a leggere per scoprirlo!

## Come fare

Per leggere gli argomenti dalla riga di comando in Arduino, dovrai utilizzare la funzione `Serial.readString()`. Questa funzione consente di leggere una stringa di testo dalla porta seriale del tuo dispositivo. Ecco un semplice esempio di codice che mostra come utilizzare questa funzione:

```Arduino
String input = Serial.readString();
Serial.println("Hai inserito: " + input);
```

In questo esempio, la variabile `input` conterrà la stringa che è stata inserita nella riga di comando. Puoi anche utilizzare la funzione `Serial.available()` per verificare se ci sono dati disponibili sulla porta seriale prima di leggerli. Ecco un esempio che ti mostrerà come farlo:

```Arduino
if (Serial.available() > 0) {
  String input = Serial.readString();
  Serial.println("Hai inserito: " + input);
}
```

Oltre a leggere gli argomenti dalla riga di comando, puoi anche utilizzare la funzione `Serial.println()` per stamparli sulla console seriale. Ad esempio, il seguente codice stampa il messaggio "Ciao mondo!" sulla console seriale:

```Arduino
Serial.println("Ciao mondo!");
```

## Approfondimenti

Ora che hai imparato come leggere gli argomenti dalla riga di comando in Arduino, potresti essere curioso di saperne di più su questa funzionalità. Una delle cose più interessanti che puoi fare è impostare dei parametri per il tuo sketch direttamente dalla riga di comando. Ad esempio, puoi impostare un valore specifico per una variabile durante l'esecuzione del tuo programma utilizzando gli argomenti. Inoltre, puoi utilizzare questa funzionalità per creare programmi più interattivi, che consentono all'utente di inserire dati direttamente dalla riga di comando.

Inoltre, è possibile combinare la funzione `Serial.readString()` con altre funzioni di controllo degli input, come `Serial.parseInt()` per leggere input numerici. In questo modo, puoi realizzare programmi che richiedono all'utente di inserire dati numerici direttamente dalla riga di comando.

## Vedi anche

Per ulteriori informazioni sull'utilizzo di funzioni e comandi in Arduino, consulta questi utili link:

- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/)
- [Comandi della riga di comando di Arduino](https://www.arduino.cc/en/Main/CommandLineArguments)
- [Esempi di progetti con Arduino](https://create.arduino.cc/projecthub)
- [Forum di supporto di Arduino](https://forum.arduino.cc/)