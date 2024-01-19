---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Eliminare caratteri che corrispondono a un modello è quando si rimuovono caratteri specifici da una stringa utilizzando un modello definito. I programmatori lo fanno per pulire o manipolare i dati.

## Come fare:

Per rimuovere i caratteri in Java che corrispondono a un determinato modello, utilizziamo il metodo `replaceALL()`. Ecco come:

```Java
public class Main {
    public static void main(String[] args) {
        String s = "aaa.bbb-ccc!ddd";
        String pattern = "[.\\-!]";
        String cleaned = s.replaceAll(pattern, "");
        System.out.println(cleaned);
    }
}
```

Output:

```Java
aaabbbcccddd
```

In questo esempio, il modello è `"[.\\-!]"`, che corrisponde a un punto, un trattino o un punto esclamativo.

## Approfondimenti

La possibilità di rimuovere caratteri da una stringa basandosi su un modello risale ai primi giorni delle espressioni regolari. In Java, abbiamo il metodo `replaceAll()` che fornisce questa funzionalità. Altre soluzioni alternative potrebbero includere l'uso di librerie di terze parti come Apache Commons o Guava.

Un dettaglio chiave della rimozione di caratteri corrispondenti a un modello in Java è che il metodo `replaceAll()` è un metodo non distruttivo. Ciò significa che crea una nuova stringa e non modifica l'originale. Questo è in linea con il fatto che le stringhe in Java sono immutabili.

## Vedi Anche:

- [Java String replaceAll () Metodo con esempio](https://www.javatpoint.com/java-string-replaceall)
- [Metodo replaceAll () in Java](https://www.geeksforgeeks.org/replaceall-method-in-java/)
- [Espressioni regolari in Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)