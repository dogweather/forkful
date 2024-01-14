---
title:    "Arduino: Scrivere un file di testo"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione fondamentale quando si programma con Arduino. Questo semplice processo consente di inserire informazioni all'interno del codice senza dover riscrivere manualmente i dati ogni volta che il programma viene eseguito. Questo risparmia tempo e semplifica il processo di sviluppo.

## Come fare

Per scrivere un file di testo con Arduino, è necessario seguire questi semplici passaggi:

- Aprire l'IDE di Arduino sul tuo computer.
- Creare un nuovo sketch e assegnargli un nome.
- Utilizzare la funzione `File.write()` per scrivere i dati all'interno del file.
- Salvare il file con l'estensione ".txt" nella stessa cartella del tuo sketch.

Nell'esempio seguente, verrà creato un file di testo che contiene una semplice stringa di testo, utilizzando la funzione `File.write()`:

```Arduino
File myFile;
void setup() {
  myFile = SD.open("myFile.txt", FILE_WRITE);
  myFile.write("Questo è un esempio di scrittura di un file di testo con Arduino.");
  myFile.close();
}
```

Una volta eseguito il codice, il file di testo verrà creato nella stessa cartella del tuo sketch e conterrà il testo specificato.

## Approfondimento

Scrivere un file di testo con Arduino può essere utile anche per salvare dati di sensori o risultati di calcoli all'interno del programma stesso. È importante notare che la capacità di memoria di Arduino è limitata, quindi assicurati di non scrivere troppi dati all'interno del file o potresti riscontrare problemi di memoria disponibile.

Inoltre, è possibile utilizzare la funzione `File.println()` per scrivere i dati su una nuova riga all'interno del file, rendendo più facile la lettura dei dati in fase di debug.

## Vedi anche

- [Documentazione ufficiale di Arduino sulla scrittura di file di testo](https://www.arduino.cc/en/Tutorial/Files)
- [Tutorial di Adafruit su come utilizzare la funzione `File.write()`](https://learn.adafruit.com/adafruit-pir-sensor/using-the-sd-library)