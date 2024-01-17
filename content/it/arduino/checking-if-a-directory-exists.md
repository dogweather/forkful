---
title:                "Verifica l'esistenza di una directory"
html_title:           "Arduino: Verifica l'esistenza di una directory"
simple_title:         "Verifica l'esistenza di una directory"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Controllare se una directory esiste è un'azione comune per i programmatori. Questo processo consiste nell'esaminare se una certa directory esiste nel sistema operativo, e nel caso contrario, crearla. I programmatori fanno spesso questo controllo per garantire che il loro codice funzioni correttamente e per evitare errori imprevisti.

## Come fare:
Per controllare se una directory esiste in Arduino, si può utilizzare la funzione ```exists()``` della libreria SD. Di seguito un esempio di codice:

```
#include <SD.h>

if(SD.exists("nome_directory")){
  //code to be executed if the directory exists
}
else{
  //code to be executed if the directory doesn't exist
}
```

Il codice sopra utilizzerà la funzione ```exists()``` per verificare se la directory "nome_directory" esiste nella scheda SD. Se esiste, il codice all'interno del primo blocco di codice verrà eseguito. In caso contrario, il codice nel secondo blocco verrà eseguito.

## Approfondimento:
Controllare l'esistenza di una directory è diventato una pratica comune tra i programmatori grazie all'aumento del numero di dispositivi e piattaforme su cui viene eseguito il codice. Sebbene esistano alternative come la creazione programmata della directory, controllarne l'esistenza è un metodo più sicuro ed efficace. Per implementare questo controllo, si possono utilizzare diverse funzioni di libreria, come ad esempio la già citata ```exists()```. Inoltre, questo controllo può essere esteso ad altri file system oltre alla scheda SD, come per esempio la memoria flash.

## Vedi anche:
- [Documentazione ufficiale della libreria SD](https://www.arduino.cc/en/Reference/SD)
- [Esempi di codice per utilizzare la libreria SD](https://www.arduino.cc/en/Tutorial/SDFat)
- [Ulteriori informazioni sull'uso della memoria flash con Arduino](https://www.arduino.cc/en/Reference/WiFiNINAFileSystem)