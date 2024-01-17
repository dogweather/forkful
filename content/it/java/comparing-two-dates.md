---
title:                "Confrontare due date"
html_title:           "Java: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

Che cosa e perché?

Comparare due date è una pratica comune tra programmatori per confrontare la data e l'ora di due eventi diversi. Questa operazione è utile per determinare l'ordine temporale di determinati eventi o per calcolare la differenza di tempo tra di essi.

Come fare:

```java
// Definiamo due oggetti di tipo LocalDate
LocalDate dataUno = LocalDate.of(2021, 07, 30);
LocalDate dataDue = LocalDate.of(2021, 08, 10);

// Utilizziamo il metodo compareTo per confrontare le due date
int risultato = dataUno.compareTo(dataDue);

if(risultato < 0) {
  System.out.println("La prima data è precedente alla seconda data");
} else if(risultato > 0) {
  System.out.println("La prima data è successiva alla seconda data");
} else {
  System.out.println("Le due date sono uguali");
}

//Output: La prima data è precedente alla seconda data
```

Deep Dive:

Nella storia della programmazione, la gestione delle date è sempre stata una sfida. Prima dell'introduzione dei tipi di dati specifici per le date in Java, i programmatori dovevano gestire le date utilizzando rappresentazioni numeriche come il numero di giorni da una data di riferimento. Ma questo approccio non era molto preciso e portava a diversi errori di calcolo. Con l'introduzione della classe LocalDate in Java 8, la gestione delle date è diventata molto più efficiente e precisa.

Un'alternativa alla classe LocalDate è l'utilizzo della classe Date, ma è obsoleta e viene sostituita dalla classe LocalDate in quanto ha molte limitazioni come l'incapacità di accettare date precedenti al 1970 o la mancanza di metodi per manipolare le date.

Per confrontare due date, il metodo compareTo utilizza la convenzione "maggiore-uguale-minore". Se la prima data è successiva alla seconda, verrà restituito un numero positivo. Se la prima data è precedente alla seconda, verrà restituito un numero negativo. E se le due date sono uguali, verrà restituito 0.

Vale la pena notare che la classe LocalDate ha anche il metodo isAfter e isBefore che possono essere utilizzati per confrontare le date in modo più esplicito.

Vedi anche: 
- [Documentazione ufficiale di Java sulla classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial su come gestire le date in Java](https://www.codejava.net/java-core/the-java-api-for-dates-and-times)