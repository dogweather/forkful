---
title:                "Lettura degli argomenti nella riga di comando"
html_title:           "Java: Lettura degli argomenti nella riga di comando"
simple_title:         "Lettura degli argomenti nella riga di comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti essere interessato a leggere gli argomenti della riga di comando in Java. Forse stai scrivendo un'applicazione che richiede input da parte dell'utente durante l'esecuzione, oppure vuoi controllare il comportamento del tuo programma in base ai parametri inseriti dall'utente. Non importa quale sia il motivo, imparare come leggere gli argomenti della riga di comando è un'abilità fondamentale per qualsiasi programmatore Java.

## Come fare

Per leggere gli argomenti della riga di comando in Java, il primo passo è creare un oggetto di tipo `String[]`, che rappresenta tutti gli argomenti passati al programma al momento dell'esecuzione.

```
public static void main(String[] args) {
    // Esempio di input: java MyProgram uno due tre
    // args conterrà {"uno", "due", "tre"}
    System.out.println("Gli argomenti della riga di comando sono:");
    for (String arg : args) {
        System.out.println(arg);
    }
}
```

Output:
```
Gli argomenti della riga di comando sono:
uno
due
tre
```

Ora che sei in grado di accedere agli argomenti, puoi utilizzarli all'interno del tuo programma come meglio desideri. Ad esempio, puoi utilizzare un blocco `if` per verificare se un argomento specifico è stato passato e, in base a ciò, eseguire un'azione diversa. Puoi anche utilizzare il `length` dell'array `args` per controllare quanti argomenti sono stati passati.

## Approfondimento

Se vuoi approfondire ulteriormente il tema degli argomenti della riga di comando in Java, ci sono alcune cose importanti da tenere a mente. Innanzitutto, gli argomenti vengono sempre passati al programma come `String`, quindi se vuoi utilizzarli come numeri o booleani, dovrai convertirli utilizzando i metodi appropriati come `Integer.parseInt()` o `Boolean.parseBoolean()`. Inoltre, è importante considerare il caso in cui non vengano passati argomenti o vengano passati argomenti non validi, per evitare errori durante l'esecuzione del programma.

## Vedi anche

- [Documentazione ufficiale di Java sulla lettura degli argomenti della riga di comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial di Java per principianti: lettura degli argomenti della riga di comando](https://www.javatutorial.net/java-command-line-arguments)
- [Esempi di codice per leggere gli argomenti della riga di comando in Java](https://www.codejava.net/java-se/args-command-line-arguments-in-java)