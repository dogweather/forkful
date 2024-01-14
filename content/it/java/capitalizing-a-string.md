---
title:    "Java: Maiuscolare una stringa"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché
In questo post parleremo di come capitalizzare una stringa in Java. Questa è un'operazione comune nella programmazione, ad esempio durante la formattazione di dati o l'elaborazione di input dall'utente. Imparare a capitalizzare correttamente una stringa è fondamentale per scrivere codice efficiente e facile da leggere.

## Come Fare
Per capitalizzare una stringa in Java, è possibile utilizzare il metodo `toUpperCase()` della classe `String`. Questo metodo restituisce una nuova stringa con tutti i caratteri convertiti in maiuscolo. Ecco un esempio di come utilizzarlo:

``` java
String testo = "ciao mondo";
String testoCapitalizzato = testo.toUpperCase();

System.out.println(testoCapitalizzato);
```
Questo codice produrrà l'output "CIAO MONDO". Ricorda che il metodo `toUpperCase()` restituirà una nuova stringa, quindi dovrai assegnarla ad una variabile per mantenerne il valore.

## Approfondimento
Ora che abbiamo visto come capitalizzare una stringa in Java, è importante capire cosa succede dietro le quinte. Quando viene chiamato il metodo `toUpperCase()`, Java crea una nuova stringa con tutti i caratteri in maiuscolo utilizzando il sistema di codifica Unicode. Questo significa che funziona per qualsiasi alfabeto o lingua supportati da Java. Inoltre, Java tiene conto della localizzazione, quindi i caratteri speciali nelle diverse lingue verranno capitalizzati in modo appropriato.

## Vedi Anche
- [Javadoc: String.toUpperCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Tutorial Java: String manipulation](https://www.javatpoint.com/java-string)
- [How to capitalize the first letter of a string in Java](https://www.baeldung.com/java-capitalize-string)