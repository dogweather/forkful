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

## Cosa & Perché?
Leggere un file di testo è un'operazione comune nel mondo della programmazione, che consiste nel leggere i contenuti di un file di testo all'interno di un programma e manipolarli in base alle esigenze. I programmatori utilizzano questa tecnica per svariati motivi, ad esempio per analizzare dati, elaborare informazioni o leggere configurazioni.

## Come fare:
```java
// Leggi un file di testo
File file = new File("test.txt"); // sostituisci con il tuo file di testo
Scanner scanner = new Scanner(file);
while (scanner.hasNextLine()) {
    String line = scanner.nextLine();
    System.out.println(line); // stampa ogni riga del file di testo
}
scanner.close();
```

Output:
```
Questa è la prima riga del file di testo.
Questa è la seconda riga.
E questa è l'ultima.
```

## Approfondimento:
La lettura di file di testo è una pratica comune nella programmazione sin dagli albori, quando le informazioni venivano memorizzate principalmente in questo formato. Oggi ci sono varie alternative alla lettura dei file di testo, come ad esempio i database o l'utilizzo di framework che semplificano la lettura dei dati.

Per implementare la lettura di un file di testo, è necessario comprendere il concetto di buffer, ovvero una memoria temporanea che contiene i dati letti dal file di testo prima che vengano elaborati dal programma. Inoltre, bisogna gestire correttamente gli errori durante la lettura del file, come ad esempio la mancanza di permessi o il file non esistente.

## Vedi anche:
- Esempi di lettura di file di testo in Java su [GitHub](https://github.com/topics/java-file-reading)
- Tutorial dettagliato sulla gestione dei file di testo in Java su [GeeksforGeeks](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)