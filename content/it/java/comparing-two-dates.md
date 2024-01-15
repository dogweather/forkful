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

## Perché confrontare due date in Java

Comparare due date è un'operazione comune nella programmazione in Java. Spesso viene utilizzata per verificare la validità di una data o per ordinarle in una sequenza cronologica. In questo articolo, ti mostrerò come confrontare le date in modo semplice ed efficiente utilizzando il linguaggio di programmazione Java.

## Come fare

Per confrontare date in Java, è necessario utilizzare la classe `LocalDate` della libreria standard `java.time`. Questa classe rappresenta una data senza informazioni di fuso orario o di ora. Ecco un esempio di come dichiarare due oggetti `LocalDate` e confrontarli:

```Java
LocalDate data1 = LocalDate.of(2021, 3, 8);
LocalDate data2 = LocalDate.of(2021, 3, 10);

//confronto tra le due date
int differenza = data2.compareTo(data1);

if (differenza > 0) {
    System.out.println("Data2 è successiva a Data1");
} else if (differenza < 0) {
    System.out.println("Data1 è successiva a Data2");
} else {
    System.out.println("Le date sono uguali");
}
```

Nell'esempio sopra, utilizziamo il metodo `compareTo()` che restituisce un valore negativo se la prima data è precedente a quella confrontata, un valore positivo se è successiva e 0 se le date sono uguali.

Possiamo anche effettuare altri tipi di confronti, come ad esempio il confronto tra i soli giorni, mesi o anni di due date, utilizzando i metodi `isEqual()`, `isBefore()` e `isAfter()` rispettivamente.

```Java
//confronto solo dei giorni
boolean stessoGiorno = data1.getDayOfMonth() == data2.getDayOfMonth();
```

## Approfondimento

Durante il confronto tra due date, è importante considerare diversi aspetti. Ad esempio, la classe `LocalDate` rappresenta solo una data, senza alcuna informazione sulla zona oraria o sull'ora effettiva. Se si desidera confrontare date con informazioni di fuso orario, è possibile utilizzare la classe `ZonedDateTime`.

Inoltre, i metodi `compareTo()` e `isEqual()` effettuano un confronto basato su valori interi, quindi è importante gestire correttamente gli errori di eventuali date non valide.

## Vedi anche

- Tutorial sulle classi `LocalDate` e `ZonedDateTime` in Java: [https://docs.oracle.com/javase/tutorial/datetime/overview/index.html](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- Esempi di confronto tra date in Java: [https://www.baeldung.com/java-date-compare](https://www.baeldung.com/java-date-compare)