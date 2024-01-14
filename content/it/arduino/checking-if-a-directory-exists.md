---
title:                "Arduino: Verifica se una directory esiste"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Perché controllare l'esistenza di una directory in Arduino
Come programmatori Arduino, spesso ci troviamo a gestire una grande quantità di file e dobbiamo essere sicuri che determinate directory esistano prima di fare operazioni su di esse. In questo articolo, impareremo come controllare se una directory esiste e gestire le eccezioni in modo efficiente.

## Come farlo
Per controllare l'esistenza di una directory in Arduino, possiamo utilizzare la funzione `exists()` della libreria `SD` (SD Card). Questa funzione restituisce un valore booleano (vero o falso) a seconda che la directory esista o meno. Vediamo un esempio:

```Arduino
#include <SD.h>

if(SD.exists("directory/")) {
  // la directory esiste, possiamo fare operazioni su di essa
  // ad esempio:
  File file = SD.open("directory/file.txt");
}
```

Se la directory esiste, possiamo anche aprire un file all'interno di essa utilizzando la funzione `open()` della libreria `SD`. In questo modo, evitiamo di incorrere in errori durante l'accesso a una directory che non esiste.

## Approfondimento
In Arduino, le directory vengono gestite tramite la libreria `SD`. Questa libreria ci consente di comunicare con una scheda SD e di leggere e scrivere file su di essa. Quando si utilizza la funzione `exists()`, Arduino cerca la directory all'interno della scheda SD e restituisce un valore booleano in base al risultato.

È importante notare che la funzione `exists()` può anche essere utilizzata per controllare l'esistenza di file. Basta passare il nome del file invece del nome della directory come parametro. Ad esempio, `SD.exists("file.txt")` restituirà un valore booleano in base alla presenza o meno del file "file.txt" all'interno della scheda SD.

## Vedi anche
- La documentazione ufficiale di Arduino sulla libreria `SD`: https://www.arduino.cc/en/Reference/SD
- Un tutorial su come gestire le directory in Arduino: https://www.arduino.cc/en/Tutorial/Files
- Altri esempi di codice con la libreria `SD`: https://www.arduino.cc/en/Tutorial/FilesFileList