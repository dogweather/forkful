---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
date:                  2024-01-20T17:42:34.484093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
In Java, cancellare caratteri che corrispondono a un modello significa usare espressioni regolari per identificare e rimuovere sequenze specifiche di caratteri da una stringa. Lo facciamo per validare l'input, pulire i dati, o manipolare testo per varie necessità del codice.

## Come fare:
```java
public class RimozioneCaratteri {
    public static void main(String[] args) {
        String fraseOriginale = "Ciao, M0nd0! Benvenut1 in Java.";
        String frasePulita = fraseOriginale.replaceAll("[0-9]", "");

        System.out.println("Frase originale: " + fraseOriginale);
        System.out.println("Frase dopo la rimozione dei numeri: " + frasePulita);
    }
}
```
Output:
```
Frase originale: Ciao, M0nd0! Benvenut1 in Java.
Frase dopo la rimozione dei numeri: Ciao, Mondo! Benvenuti in Java.
```

## Approfondimento
Storicamente, le espressioni regolari derivano dalla teoria degli automi e sono state introdotte nei programmi di computer negli anni '50. Java le implementa tramite la classe `Pattern` e la classe `Matcher`. Un'alternativa al metodo `replaceAll()` è `replaceFirst()`, per sostituire solo la prima occorrenza. In termini di implementazione, `replaceAll()` compila l'espressione regolare in un pattern e crea un matcher per trovare le corrispondenze, che poi vengono sostituite nel testo originale. È importante notare che l'operazione di rimozione può essere costosa in termini di performance se applicata a stringhe molto grandi o a pattern complessi.

## Vedere anche
- [Documentazione ufficiale Java Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Tutorial Oracle su espressioni regolari](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Stack Overflow: Discussioni su espressioni regolari in Java](https://stackoverflow.com/questions/tagged/regex+java)