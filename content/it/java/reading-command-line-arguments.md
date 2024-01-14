---
title:    "Java: Leggere gli argomenti della linea di comando"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché leggere gli argomenti da riga di comando in Java

Leggere gli argomenti da riga di comando è un'abilità importante per ogni programmatore Java. Quando si scrivono applicazioni, spesso è necessario fornire input all'applicazione senza dover modificare il codice sorgente. Leggere gli argomenti da riga di comando consente di fornire input dinamico senza dover ricompilare il codice ogni volta.

## Come farlo: esempi di codice e output

Per leggere gli argomenti da riga di comando in Java, utilizziamo la classe `java.lang.System` e il suo metodo `getProperty`. Vediamo un esempio di codice:

```Java
public class CommandLineArgumentsExample {

    public static void main(String[] args) {
        String property = System.getProperty("arg1");
        System.out.println(property);
    }
}
```

Se eseguiamo questo codice con il seguente comando `java CommandLineArgumentsExample -Darg1="Hello World!"` otterremo l'output `Hello World!` nel terminale.

## Approfondimento: informazioni dettagliate su come leggere gli argomenti da riga di comando

Quando si lavora con gli argomenti da riga di comando, è importante essere consapevoli dei seguenti aspetti:

- Gli argomenti da riga di comando devono essere passati come opzioni di sistema con il prefisso `-D`.
- Possiamo leggere più argomenti da riga di comando utilizzando il metodo `getProperty` più volte.
- È possibile definire valori predefiniti per gli argomenti da riga di comando dal codice sorgente utilizzando `getProperty` con due parametri.
- È possibile utilizzare il metodo `clearProperty` per rimuovere un argomento da riga di comando.

## Vedi anche

- Documentazione ufficiale di Oracle su [Passing Command Line Arguments to a Java Program](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Tutorial di Baeldung su [Reading Command Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)
- Esempi di codice su [GitHub](https://github.com/search?q=java+command+line+arguments)