---
title:    "Java: Concatenazione di stringhe"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché Concatenare le Stringhe

In Java, le stringhe sono un concetto fondamentale e sono usate molto frequentemente nelle applicazioni. Spesso ci troveremo nella situazione in cui dobbiamo unire due o più stringhe per creare una stringa più lunga. Per fare ciò, possiamo usare il metodo `concat()`.

## Come Concatenare le Stringhe

La sintassi del metodo `concat()` è la seguente:

```Java
String nuova_stringa = stringa1.concat(stringa2);
```

Possiamo anche usare l'operatore `+` per concatenare le stringhe, che è più breve e facile da leggere:

```Java
String nuova_stringa = stringa1 + stringa2;
```

Ecco un esempio di entrambi i metodi in azione:

```Java
String saluto = "Ciao";
String nome = "Marco";

// Usando il metodo concat()
String messaggio1 = saluto.concat(nome); // risultato: "CiaoMarco"

// Usando l'operatore +
String messaggio2 = saluto + nome; // risultato: "CiaoMarco"
```

Come possiamo vedere, entrambi i metodi danno lo stesso risultato, ma l'operatore `+` sembra essere più conveniente e più comunemente usato nella pratica.

## Approfondimento sulla Concatenazione delle Stringhe

È importante notare che quando si concatenano molte stringhe, è consigliabile utilizzare un `StringBuilder` invece dei metodi menzionati sopra. Questo perché una `StringBuilder` gestisce la creazione di stringhe più efficientemente, risparmiando memoria e tempo di esecuzione.

Un altro aspetto importante da considerare è che le stringhe in Java sono immutabili, il che significa che non possono essere modificate una volta create. Ciò significa che ogni volta che si esegue una concatenazione di stringhe, in realtà si crea una nuova stringa invece di modificare la stringa originale.

## Vedi Anche

- Java Doc sul metodo concat(): https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#concat(java.lang.String)
- Tutorial su StringBuilder: https://www.baeldung.com/java-stringbuilder
- Spiegazione delle stringhe immutabili in Java: https://www.baeldung.com/java-immutable-strings