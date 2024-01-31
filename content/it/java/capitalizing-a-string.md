---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutte le lettere in maiuscole. Lo facciamo per uniformità nei testi, per enfatizzare o per rispettare certi standard (es. codici).

## How to:
Ecco come farlo in Java:

```java
public class Capitalizer {
    public static void main(String[] args) {
        String myString = "ciao mondo!";
        String capitalizedString = myString.toUpperCase();
        System.out.println(capitalizedString); // Output: CIAO MONDO!
    }
}
```

Facile, no?

## Deep Dive
La capitalizzazione delle stringhe esiste da quando abbiamo avuto bisogno di standardizzare dati per confronti o visualizzazioni. In Java, il metodo `toUpperCase()` è disponibile fin dalla versione 1.0, semplice e diretto. Ma attenzione: bisogna gestire la localizzazione, perché alcuni caratteri cambiano in maiuscolo in modo diverso a seconda della lingua. Alternativa? Apache Commons Lang e la classe `StringUtils` con metodi come `capitalize()` o `upperCase()` che considerano già questi aspetti.

Implementazione:
- `toUpperCase()` utilizza la classe `Character` per convertire ogni carattere.
- Locale-sensitive: `toUpperCase(Locale locale)` per rispettare regole linguistiche specifiche.

## See Also:
- [String documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Apache Commons Lang – StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [Locale Class in Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
