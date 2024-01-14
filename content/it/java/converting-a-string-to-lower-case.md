---
title:    "Java: Convertire una stringa in minuscolo"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché

Le stringhe sono uno dei tipi di dati più comuni nel linguaggio di programmazione Java e spesso si ha la necessità di manipolarle per adattarle alle nostre esigenze. Convertire una stringa in minuscolo è un'operazione utile quando si desidera uniformare il testo presente in una stringa per confronti o manipolazioni.

## Come fare

```Java 
String frase1 = "QUESTA è una stringa";
String frase2 = frase1.toLowerCase();
System.out.println(frase2);
```

Output:
```
questa è una stringa
```

Per convertire una stringa in minuscolo in Java, si utilizza il metodo `toLowerCase()`. Questo metodo restituisce una nuova stringa con tutti i caratteri convertiti in minuscolo. Nel nostro esempio, abbiamo creato una variabile `frase1` contenente una stringa con una combinazione di caratteri maiuscoli e minuscoli. Utilizzando il metodo `toLowerCase()`, abbiamo creato una nuova stringa `frase2` in cui tutti i caratteri sono stati convertiti in minuscolo. Infine, la stringa `frase2` viene stampata a video con il metodo `println()`.

## Approfondimento

In Java, le stringhe sono immutabili, il che significa che una volta create non possono essere modificate. Quando si utilizza il metodo `toLowerCase()`, viene creata una nuova stringa e la stringa originale rimane invariata. Inoltre, è importante ricordare che la conversione in minuscolo dipende dal sistema operativo e dalla lingua impostata sulla macchina in cui si esegue il codice.

## Vedi Anche

- [La classe String in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Metodo toLowerCase() in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Manipolazione delle stringhe in Java: tutorial per principianti](https://www.baeldung.com/java-string)