---
title:                "Concatenando stringhe."
html_title:           "Java: Concatenando stringhe."
simple_title:         "Concatenando stringhe."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Puoi chiederti perché dovresti leggere un articolo su come concatenare stringhe in Java. La risposta è semplice: la concatenazione di stringhe è una delle operazioni di base più comuni e utili nella programmazione, e conoscere il modo corretto di farlo può rendere il tuo lavoro più efficiente e organizzato.

## Come Fare

La concatenazione di stringhe in Java può sembrare un concetto semplice, ma ci sono alcuni punti da tenere a mente per evitare errori e migliorare le performance. Ecco due modi per concatenare stringhe in Java:

### Utilizzando l'operatore +

```Java
String str1 = "Ciao";
String str2 = "Mondo";
String risultato = str1 + str2;

System.out.println(risultato); // Output: CiaoMondo
```

In questo esempio, abbiamo utilizzato l'operatore di concatenazione `+` per unire le due stringhe `str1` e `str2` in una nuova stringa `risultato`.

### Utilizzando il metodo `concat()`

```Java
String str1 = "Ciao";
String str2 = "Mondo";
String risultato = str1.concat(str2);

System.out.println(risultato); // Output: CiaoMondo
```

In questo secondo esempio, abbiamo utilizzato il metodo `concat()` della classe String per ottenere lo stesso risultato.

Ora, sei libero di utilizzare il metodo che preferisci, questo dipende dalle tue preferenze e dai tuoi requisiti.

## Approfondimento

Ci sono alcune cose su cui vale la pena di soffermarsi riguardo alla concatenazione di stringhe in Java. La prima è che l'uso dell'operatore `+` è più semplice e leggibile, ma può essere meno efficiente in termini di performance, soprattutto se si concatenano molte stringhe in sequenza. In questo caso, è meglio utilizzare il metodo `concat()` o la classe `StringBuilder`, che viene ottimizzata per la manipolazione di stringhe.

Inoltre, è importante tenere presente che le stringhe in Java sono immutabili, il che significa che ogni volta che viene eseguita un'operazione di concatenazione, viene creata una nuova stringa invece di modificarne una esistente. Ciò può portare a un utilizzo inefficiente della memoria se non si gestisce correttamente.

In generale, è sempre una buona pratica utilizzare il metodo `concat()` o la classe `StringBuilder` quando si devono concatenare più di due stringhe in sequenza.

## Vedi anche

- [Documentazione ufficiale di Java sulla classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial su StringBuilder in Java](https://www.baeldung.com/java-stringbuilder)
- [Guida introduttiva a Java](https://www.javatpoint.com/java-tutorial)