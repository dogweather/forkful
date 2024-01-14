---
title:                "Java: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Lettura di un file di testo è una delle operazioni fondamentali nella programmazione Java. È utile per accedere alle informazioni contenute in un file, come ad esempio i dati di un utente o delle configurazioni per un programma. In questo post, esploreremo come leggere un file di testo utilizzando Java.

## Come farlo

Per prima cosa, dobbiamo creare un'istanza della classe File utilizzando il percorso del file che vogliamo leggere. In questo esempio, abbiamo creato un file chiamato "test.txt" nella nostra cartella di progetto.

```Java
File file = new File("test.txt");
```

Successivamente, dobbiamo creare un'istanza della classe Scanner per poter leggere il file. Il costruttore della classe Scanner prende come parametro l'istanza della classe File che abbiamo creato prima.

```Java
Scanner scanner = new Scanner(file);
```

Ora siamo pronti per leggere il file. Utilizzando il metodo `hasNextLine()`, possiamo controllare se ci sono ancora delle righe da leggere nel file e utilizzando il metodo `nextLine()` possiamo ottenere la riga successiva.

```Java
while (scanner.hasNextLine()) {
    String line = scanner.nextLine();
    System.out.println(line);
}
```

Il codice sopra stamperà ogni riga del file sul terminale. Ovviamente, puoi fare qualsiasi altra cosa con i dati ottenuti dalle righe, come ad esempio salvarli in un array o elaborarli in qualche modo.

## Approfondimento

Ci sono alcuni dettagli da considerare quando si legge un file di testo con Java. Ad esempio, è importante gestire le eccezioni che possono essere generate dalla lettura del file. Inoltre, è possibile specificare l'encoding del file quando si crea l'istanza della classe Scanner, se si sa che il file contiene caratteri non standard.

Altre classi utili per la lettura di file di testo sono `FileReader` e `BufferedReader`, che offrono funzionalità aggiuntive come la possibilità di specificare la dimensione del buffer o semplicemente di leggere un carattere alla volta.

## Vedi anche

- [Documentazione ufficiale di Java sulle operazioni di input/output](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
- [Tutorial su come leggere un file di testo utilizzando Java](https://www.baeldung.com/java-read-file)
- [Domande frequenti su Java I/O su Stack Overflow](https://stackoverflow.com/questions/tagged/java+io)