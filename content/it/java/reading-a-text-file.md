---
title:                "Lettura di un file di testo"
html_title:           "Java: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato a sviluppare applicazioni o a diventare un programmatore Java esperto, capire come leggere un file di testo è un passo fondamentale. Questo ti permette di acquisire dati da fonti esterne e incorporarli nella tua applicazione.

## Come fare

Per leggere un file di testo in Java, segui questi semplici passaggi:

- Apri un oggetto `FileInputStream` che rappresenta il file da leggere.
- Crea un oggetto `BufferedReader` che leggerà i dati dallo `InputStream`.
- Leggi il file riga per riga utilizzando `BufferedReader` finché non raggiungi la fine del file.
- Chiudi lo `InputStream` e lo `BufferedReader` per rilasciare le risorse.

```Java
public static void main(String[] args) {
  try {
    //Apre il file da leggere
    FileInputStream fis = new FileInputStream("esempio.txt");

    //Crea un oggetto BufferedReader
    BufferedReader br = new BufferedReader(new InputStreamReader(fis));

    //Leggi riga per riga fino alla fine del file
    String line = br.readLine();
    while (line != null) {
      System.out.println(line);
      line = br.readLine();
    }
    //Chiudi lo stream e il buffer
    br.close();
    fis.close();
  } catch (IOException e) {
    e.printStackTrace();
  }
}
```

### Deep Dive

Oltre alla semplice lettura di un file di testo, Java fornisce anche alcune opzioni avanzate per gestire i dati. Puoi utilizzare le classi `Scanner` o `FileReader` per leggere il file e salvare i dati in diverse variabili per un accesso più facile. Inoltre, puoi anche manipolare i dati mentre li leggi, come ad esempio cercare parole specifiche o eseguire operazioni di calcolo sui numeri letti.

Se vuoi imparare di più sull'utilizzo dei file in Java, puoi anche esplorare le opzioni di scrittura e di manipolazione dei file.

## Vedi anche

- [Tutorial di Java per principianti](https://www.w3schools.com/java/)
- [Documentazione ufficiale di Java](https://docs.oracle.com/javase/10/)
- [Altri tutorial su Java](https://www.tutorialspoint.com/java/index.htm)