---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Trasformare una stringa in minuscolo in Java 

## Cos'è e Perché?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfanumerici di una stringa in lettere minuscole. I programmatori lo fanno per eseguire confronti tra stringhe senza tener conto della capitalizzazione o per uniformare l'input dell'utente.

## Come fare:

Ecco un esempio di come convertire una stringa in minuscolo in Java:

```Java
public class Main {
    public static void main(String[] args) {
        String s = "Java È Fantastico!";
        String lowerCase = s.toLowerCase();
        System.out.println(lowerCase);
    }
}
```

Questo produrrà il seguente output quando viene eseguito:

```Java
java è fantastico!
```

## Approfondimento

La funzione `toLowerCase()` esiste in Java da quando la classe `String` è stata introdotta per la prima volta, come parte del JDK 1.0 nel 1996. Funziona convertendo ogni carattere alfanumerico nella stringa in minuscolo.

Non c'è un'alternativa diretta a `toLowerCase()` in Java standard. Tuttavia, alcune librerie esterne, come Apache Commons e Guava, forniscono funzioni simili con alcune funzionalità aggiuntive.

Quando converti una stringa in minuscolo con `toLowerCase()`, Java itera su ogni carattere della stringa e chiama un metodo nativo per convertirlo in minuscolo. Se il carattere non ha una rappresentazione in minuscolo (come un simbolo o un numero), allora rimane invariato.

## Vedi Anche

Per approfondire l'argomento, visita i seguenti link:

- Documentazione Java Oracle per la funzione [toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- Un confronto dettagliato tra `toLowerCase()` e altre funzioni simili offerte da librerie esterne può essere trovato in [questo post](https://stackoverflow.com/questions/11583091/java-string-tolowercase-and-touppercase-comparison/)