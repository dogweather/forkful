---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Gli argomenti della riga di comando sono stringhe di informazioni che vengono trasmesse a un programma Java quando viene avviato. La gestione di questi argomenti consente agli sviluppatori di personalizzare il comportamento del programma in base a specifiche esigenze dell'utente alla esecuzione.

## Come fare:

Ecco un esempio di base su come leggere gli argomenti della riga di comando in Java:

```Java
public class Main {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println("Argomento: " + arg);
        }
    }
}
```

Dovresti eseguire il programma con argomenti da riga di comando aggiuntivi:

```shell
java Main Ciao Mondo
```

E guarda l'uscita:

```shell
Argomento: Ciao
Argomento: Mondo
```

## Approfondimento

Gli argomenti da riga di comando sono una caratteristica storica essenziale dell'interazione tra utente e software, risalente ai primi giorni della programmazione.

Come alternativa alla lettura diretta degli argomenti da riga di comando, è possibile utilizzare librerie Java come Apache Commons CLI o JCommander che forniscono API più ricche per l'analisi degli argomenti.

Gli argomenti della riga di comando sono rappresentati come un array di stringhe (String[] args) nel metodo main di Java. Java suddivide automaticamente gli argomenti forniti sulla riga di comando in base agli spazi e li assegna all'array args.

## Vedi anche:

Per un'analisi più dettagliata e completa, dai un'occhiata a questi link:
- [Oracle Docs - argomenti della riga di comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Baeldung - Guida agli argomenti della riga di comando in Java](https://www.baeldung.com/java-command-line-arguments)