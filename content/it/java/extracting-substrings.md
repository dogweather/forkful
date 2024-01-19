---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?
Estrarre sottotringhe (substring) significa prelevare un pezzo o una sezione più piccola da una stringa più grande in Java. I programmatori lo fanno per vari motivi: ricerca di pattern, elaborazione di testi, oppure per l'isolamento di informazioni specifiche all'interno di un testo più ampio.

## Come fare:
Ecco un esempio di codice per estrarre una substring in Java:

```Java
public class EsempioSubstring {
    public static void main(String[] args) {
        String frase = "Ciao mondo! Questo è un esempio.";
        String sottostringa = frase.substring(5, 10);
        System.out.println(sottostringa);
    }
}
```

Questo codice produrrà l'output seguente:

```Java
mondo
```

## Approfondimento
1. **Contesto storico**: La funzione `substring()` è presente in diversi linguaggi di programmazione, non solo in Java. La sua implementazione in Java risale alla versione 1.0.
2. **Alternative**: In Java, puoi anche usare `split()`, che divide la stringa in base a un delimitatore e restituisce un array di stringhe. Un'altra alternativa è la funzione `charAt()`, che ti permette di estrarre un carattere alla volta.
3. **Dettagli di implementazione**: In Java, `substring(int beginIndex, int endIndex)` è il metodo che puoi utilizzare per estrarre una substring. Indica il carattere di inizio (incluso) e il carattere di fine (escluso) della substring che desideri estrarre. Se metti solo un indice, prenderai la substring dall'indice specificato fino alla fine.

## Vedi anche
Queste risorse possono essere utili per approfondire:

1. **Documentazione ufficiale di Java per il metodo substring()**: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-
2. **Esempi di utilizzo della funzione substring() in Java**: https://www.javatpoint.com/java-string-substring
3. **Alternative alla funzione substring()**: https://www.geeksforgeeks.org/different-ways-for-string-to-string-conversion-in-java/ 
4. **Tutorial sul trattamento delle stringhe in Java**: https://www.w3schools.com/java/java_strings.asp