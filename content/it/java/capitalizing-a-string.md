---
title:    "Java: Capitalizzare una stringa"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con il linguaggio di programmazione Java, si trovano stringhe di testo che non sono capitalizzate correttamente. Per correggere questo problema, è necessario saper utilizzare il metodo `toUpperCase()` per le stringhe. In questo articolo, vedremo come utilizzare questo metodo per capitalizzare una stringa e approfondiremo il funzionamento dietro di esso.

## Come Fare

Per capitalizzare una stringa, è necessario utilizzare il metodo `toUpperCase()` su di essa. Questo metodo restituisce una nuova stringa con tutti i caratteri convertiti in maiuscolo. Di seguito è riportato un esempio di codice che mostra come utilizzare questo metodo:

```Java
String frase = "ciao a tutti!";
String fraseCapitalizzata = frase.toUpperCase();
System.out.println(fraseCapitalizzata);
```

L'output di questo codice sarà `CIAO A TUTTI!`. Come puoi vedere, il metodo `toUpperCase()` ha convertito tutti i caratteri della stringa in maiuscolo.

Di solito è necessario salvare il risultato del metodo `toUpperCase()` in una nuova variabile, poiché il metodo non modifica la stringa originale. Tuttavia, è anche possibile utilizzare il metodo direttamente sulla stringa originale, come nell'esempio seguente:

```Java
String frase = "ciao a tutti!";
frase = frase.toUpperCase();
System.out.println(frase);
```

In questo caso, l'output sarebbe lo stesso di prima, `CIAO A TUTTI!`.

Un'altra cosa da tenere a mente è che questo metodo funziona solo per i caratteri ASCII. Se la stringa contiene caratteri di altre lingue, il risultato potrebbe non essere quello desiderato.

## Approfondimento

Il metodo `toUpperCase()` fa parte della classe `String` di Java, che è una classe built-in che rappresenta una sequenza di caratteri. Questo metodo è uno dei tanti metodi disponibili nella classe `String` che permette di manipolare le stringhe. 

Uno dei vantaggi di utilizzare questo metodo è che rende più semplice la gestione delle stringhe durante confronti o operazioni di ricerca, poiché converte entrambe le stringhe in maiuscolo prima di eseguire l'operazione.

Tuttavia, è importante ricordare che questo metodo crea una nuova stringa ogni volta che viene utilizzato, il che potrebbe causare problemi di performance durante l'esecuzione di operazioni complesse su molte stringhe. In questo caso, potrebbe essere più efficace utilizzare un buffer di caratteri o una stringa modificabile.

## Vedi Anche

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial su Java String](https://www.geeksforgeeks.org/java-lang-string-class-in-java/)
- [Spiegazione del metodo toUpperCase()](https://stackoverflow.com/questions/34595530/what-does-method-to-uppercase-do-in-java)