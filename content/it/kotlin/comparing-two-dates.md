---
title:                "Kotlin: Confrontare due date"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diversi motivi per cui potresti essere interessato a confrontare due date in Kotlin. Ad esempio, potresti voler verificare se una data è successiva o precedente rispetto a un'altra, o se due date corrispondono esattamente.

## Come fare

Per confrontare due date in Kotlin, puoi utilizzare la funzione `compareTo()` della classe `LocalDate`. Ad esempio, se vogliamo confrontare due date create a partire dai valori delle loro componenti (anno, mese, giorno), possiamo farlo in questo modo:

```
Kotlin val data1 = LocalDate.of(2021, 5, 15)
val data2 = LocalDate.of(2021, 5, 20)

// Confronto tra le due date
val risultato = data1.compareTo(data2)
println(risultato)
```

In questo caso, il risultato sarà -5 poiché la data1 è precedente alla data2 di 5 giorni.

Puoi anche utilizzare il metodo `isEqual()` per verificare se due date corrispondono esattamente, come mostrato nell'esempio seguente:

```
Kotlin val data1 = LocalDate.of(2021, 5, 15)
val data2 = LocalDate.of(2021, 5, 15)

// Verifica se le due date sono uguali
val confronta = data1.isEqual(data2)
println(confronta)
```

In questo caso, il risultato sarà `true` poiché le due date corrispondono esattamente.

## Approfondimento

La funzione `compareTo()` confronta due date confrontando innanzitutto l'anno, poi il mese e infine il giorno. Se l'anno delle due date è diverso, l'operazione restituirà la differenza tra gli anni. Se invece l'anno è lo stesso ma il mese è diverso, la differenza tra i mesi sarà restituita. Infine, se anche il mese è lo stesso ma il giorno è diverso, la differenza tra i giorni sarà il risultato dell'operazione.

Puoi anche utilizzare il metodo `isBefore()` o `isAfter()` per verificare se una data è precedente o successiva a un'altra data. Invece di restituire un valore numerico, questi metodi restituiranno un valore booleano `true` o `false`.

## Vedi anche

- [Documentazione ufficiale di Kotlin](https://kotlinlang.org/docs/datetime.html)
- [Convertire una stringa in una data in Kotlin](https://www.section.io/engineering-education/convert-string-to-date-in-kotlin/)
- [Utilizzare le funzioni di comparazione in Kotlin](https://www.geeksforgeeks.org/comparisons-made-simpler-in-kotlin/)
- [Esempi di confronto tra date in Kotlin](https://www.codota.com/code/java/methods/java.time.LocalDate/compareTo)