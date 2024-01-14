---
title:                "Java: Utilizzando le espressioni regolari"
simple_title:         "Utilizzando le espressioni regolari"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, nella programmazione, è necessario cercare informazioni all'interno di una stringa o effettuare sostituzioni in modo veloce ed efficiente. Le espressioni regolari offrono un modo potente per farlo, consentendo di trovare e manipolare i dati utilizzando pattern predefiniti.

## Come Utilizzare

Per utilizzare le espressioni regolari in Java, è necessario utilizzare la classe `Pattern` e `Matcher` del pacchetto `java.util.regex`. Qui di seguito, mostro come trovare un numero di telefono all'interno di una stringa e sostituirlo con un numero diverso:

```
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexDemo {

    public static void main(String[] args) {
        String str = "Il mio numero di telefono è 555-123-4567.";
        String regex = "(\\d{3})-(\\d{3})-(\\d{4})";
        String replace = "555-555-5555";

        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(str);

        System.out.println("Stringa originale: " + str);
        str = matcher.replaceFirst(replace);
        System.out.println("Stringa modificata: " + str);
    }
}
```

Questo codice utilizza un'espressione regolare per trovare il numero di telefono all'interno della stringa e lo sostituisce con il nuovo numero. L'output sarebbe il seguente:

```
Stringa originale: Il mio numero di telefono è 555-123-4567.
Stringa modificata: Il mio numero di telefono è 555-555-5555.
```

## Approfondimento

Le espressioni regolari possono sembrare un po' complicate a prima vista, ma una volta capito il concetto di base, sono molto utili. I pattern predefiniti, come `\\d` per trovare i numeri, rendono facile trovare e manipolare dati specifici all'interno di una stringa. Inoltre, le espressioni regolari possono essere utilizzate in molte altre lingue di programmazione oltre a Java, rendendole uno strumento comune e versatile per i programmatori.

## Vedi Anche

- Java API per le espressioni regolari: https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html
- Tutorial su espressioni regolari in Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Sintassi di base delle espressioni regolari: https://www.javatpoint.com/java-regex