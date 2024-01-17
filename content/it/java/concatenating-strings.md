---
title:                "Concatenazione di stringhe"
html_title:           "Java: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La concatenazione di stringhe è un'operazione comune nella programmazione Java, che consiste nell'unire più stringhe per creare una nuova stringa. I programmatori spesso la utilizzano per formattare testi, creare output più significativi o semplicemente per combinare informazioni diverse.

## Come fare:
```Java
// Concatenazione di stringhe utilizzando l'operatore +
String s1 = "Ciao";
String s2 = "mondo!";
String s3 = s1 + " " + s2;

// Concatenazione con il metodo concat()
String s4 = s1.concat(" ").concat(s2);

// Output: Ciao mondo!
System.out.println(s3);
System.out.println(s4);
```
È possibile utilizzare sia l'operatore `+` che il metodo `concat()` per concatenare stringhe in Java. Entrambi eseguono la stessa operazione e restituiscono una nuova stringa che rappresenta la concatenazione delle stringhe di input. Tuttavia, l'uso dell'operatore `+` è più comune in quanto è più semplice e leggibile.

## Approfondimento:
La concatenazione di stringhe ha origini nel linguaggio di programmazione Basic, dove era la principale operazione per manipolare stringhe. Tuttavia, in linguaggi più moderni come Java, ci sono alternative più efficienti per gestire operazioni su stringhe, come l'utilizzo di classi come `StringBuilder` o `StringBuffer`.

Inoltre, è importante tenere presente che la concatenazione di stringhe in Java non è un'operazione efficiente in termini di performance, in quanto ogni concatenazione crea una nuova stringa e questo può essere costoso in termini di memoria. Per questo motivo, l'utilizzo di classi come `StringBuilder` è preferibile quando si hanno molte concatenazioni di stringhe.

## Vedi anche:
- [Documentazione Java - Classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Documentazione Java - Classe StringBuilder](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Documentazione Java - Classe StringBuffer](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)