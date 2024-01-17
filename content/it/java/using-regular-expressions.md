---
title:                "Utilizzare le espressioni regolari"
html_title:           "Java: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

Cosa e perché?

Le espressioni regolari sono uno strumento molto utile per i programmatori. Sono una forma di stringa che viene usata per trovare e manipolare testi all'interno di un codice. I programmatori le utilizzano per semplificare il processo di ricerca e sostituzione di stringhe all'interno di un programma.

Come fare:

```Java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegExExample {
  public static void main(String[] args) {

    // Definiamo una stringa in cui cercare
    String testo = "Benvenuti alla guida di Java";

    // Definiamo la regola da cercare
    String regex = "Java";

    // Creiamo un oggetto Pattern utilizzando il metodo compile()
    Pattern pattern = Pattern.compile(regex);

    // Usiamo l'oggetto Pattern per ottenere un oggetto Matcher
    Matcher matcher = pattern.matcher(testo);

    // Utilizziamo il metodo find() per effettuare la ricerca all'interno della stringa
    if(matcher.find()){
        System.out.println("Stringa trovata!");
    }
    else{
        System.out.println("Stringa non trovata!");
    }

  }
}

```

Output: Stringa trovata!

Deep Dive:

Le espressioni regolari hanno avuto origine negli anni '50 ed è stato il matematico Stephen Kleene ad introdurre il concetto di automi a stati finiti per rappresentarle. Sono diventate popolari negli anni '70 grazie all'utilizzo nei sistemi di editor di testo. Esistono anche alternative alle espressioni regolari, come ad esempio l'utilizzo delle substringhe o del metodo contains() in Java. L'implementazione delle espressioni regolari in Java si basa sulla classe Pattern e sulla classe Matcher, che permettono di definire una regola e utilizzarla per cercare, sostituire o estrarre stringhe.

Vedi anche:

Per ulteriori informazioni e approfondimenti sulle espressioni regolari in Java, visita i seguenti link:

- Documentazione ufficiale di Java su espressioni regolari: https://docs.oracle.com/javase/7/docs/api/java/util/regex/package-summary.html 
- Tutorial su espressioni regolari in Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Un sito che permette di testare espressioni regolari: https://regex101.com/