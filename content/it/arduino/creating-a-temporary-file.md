---
title:    "Arduino: Creazione di un file temporaneo"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di creare un file temporaneo nel tuo progetto Arduino? Creare un file temporaneo può essere utile quando si desidera memorizzare temporaneamente dei dati o effettuare delle operazioni che richiedono l'uso di uno spazio di memoria esterno. In questa breve guida scoprirai come creare un file temporaneo nel tuo programma Arduino.

## Come

Per creare un file temporaneo nel tuo programma Arduino è necessario utilizzare la funzione `File::createTemp()`. Questa funzione creerà un file con un nome univoco nel percorso specificato e restituirà un oggetto della classe `File` che permetterà di accedere al file temporaneo. Di seguito è riportato un esempio di codice che illustra come utilizzare questa funzione:

```Arduino
// Creare un file temporaneo nella cartella di lavoro
File tempFile = File::createTemp("pasta");

// Scrivere all'interno del file temporaneo
tempFile.println("Spaghetti al pomodoro");

// Chiudere il file temporaneo
tempFile.close();
```

L'output di questo codice sarà un file con il nome "pasta.tmp" contenente il testo "Spaghetti al pomodoro". È importante notare che il file temporaneo viene automaticamente eliminato alla chiusura del programma Arduino.

## Deep Dive

La funzione `File::createTemp()` è molto utile in situazioni in cui si desidera creare un file temporaneo con un nome univoco. Tuttavia, potresti avere la necessità di specificare un percorso diverso da quello di lavoro per creare il file temporaneo. In questo caso, è possibile utilizzare la funzione `File::createTempIn()` che accetta come parametro il percorso desiderato.

Un altro aspetto da tenere in considerazione è che il nome del file temporaneo generato dalla funzione `createTemp()` è basato sul valore di `millis()`, che rappresenta il numero di millisecondi trascorsi dall'avvio del programma. Ciò significa che se il programma viene riavviato, il nome del file temporaneo sarà diverso. Se questo non è desiderabile, è possibile utilizzare la funzione `File::createTempUnique()` che genera un nome di file temporaneo basato sulla data e ora corrente.

## Vedi anche

Per ulteriori informazioni su come utilizzare la funzione `File::createTemp()`, puoi consultare la documentazione ufficiale di Arduino: https://www.arduino.cc/reference/en/libraries/file/create/

Se desideri approfondire la gestione dei file su Arduino, puoi leggere questo articolo su come utilizzare la libreria SD per salvare dati su una scheda SD: https://www.arduino.cc/en/reference/SD

E se hai dei dubbi o vuoi condividere le tue esperienze con la creazione di file temporanei su Arduino, puoi unirti alla community di Arduino su GitHub: https://github.com/arduino/arduino-cli

Grazie per aver letto questo articolo e speriamo che ti sia stato utile! Buon divertimento con Arduino!