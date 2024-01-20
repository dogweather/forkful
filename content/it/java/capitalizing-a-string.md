---
title:                "Mettere in maiuscolo una stringa"
html_title:           "Java: Mettere in maiuscolo una stringa"
simple_title:         "Mettere in maiuscolo una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno per migliorare la leggibilità e per formattare correttamente i dati in output.

## Come si fa:

Guardiamo un esempio pratico con il codice Java: 

```java
public class Main {
    public static void main(String[] args) {
        String s = "buongiorno mondo!";
        String capitalized = s.substring(0, 1).toUpperCase() + s.substring(1);
        System.out.println(capitalized);
    }
}
```

Quando eseguiamo questo programma, l'output sarà:
```
Buongiorno mondo!
```

## Approfondimento

1. _Contesto storico:_ L’uso di stringhe capitalizzate è stato amp&#8203;la&#8203;mente utilizzato fin dalle prime fasi dello sviluppo del linguaggio di programmazione.

2. _Alternative:_ Esistono vari modi per capitalizzare una stringa in Java. Un altro metodo popolare è utilizzare la libreria esterna Apache Commons Lang, che offre metodi più sofisticati laddove il metodo di base non è sufficiente.

3. _Dettagli di implementazione:_ Nell'esempio, abbiamo utilizzato i metodi substring e toUpperCase di Java. Il metodo substring viene utilizzato per suddividere la stringa in due parti. Il metodo toUpperCase è poi usato per trasformare la prima lettera in maiuscolo.

## Vedere anche

- Per saperne di più sull'uso delle stringhe in Java, consulta [String (Java Platform SE 14)](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- Per gli alternative metodi di capitalizzare una stringa, consulta [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- Per ulteriori dettagli sull'uso dei metodi substring e toUpperCase in Java, consulta [Substring (Java Platform SE 14)](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#substring(int)) e [ToUpperCase (Java Platform SE 14)](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#toUpperCase())