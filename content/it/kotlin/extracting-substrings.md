---
title:    "Kotlin: Estrazione di sottostringhe"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre delle sottostringhe può essere un'operazione molto utile quando si lavora con i dati in un programma Kotlin. Con questa tecnica, è possibile accedere a parti specifiche di una stringa e manipolarle in diversi modi. In questo articolo, impareremo come sfruttare la funzione `substring()` per estrarre delle sottostringhe in Kotlin.

## Come Fare

Per estrarre una sottostringa in Kotlin, è necessario utilizzare il metodo `substring()` su una stringa esistente. Questo metodo accetta due parametri: l'indice di inizio e l'indice di fine della sottostringa desiderata.

```
Kotlin val str = "Ciao a tutti!" val subs = str.substring(5,9) println(subs)
```

Questo codice restituirebbe `a tu`, in quanto partirà dall'indice 5 (incluso) e arriverà fino all'indice 9 (escluso) della stringa originale.

È importante notare che gli indici delle stringhe in Kotlin iniziano da zero. Ciò significa che il primo carattere ha indice 0, il secondo carattere ha indice 1 e così via. Si può ottenere la lunghezza di una stringa utilizzando il metodo `length`.

```
Kotlin val str = "Ciao a tutti!" val subs = str.substring(5, str.length) println(subs)
```

In questo esempio, la sottostringa inizia dall'indice 5 (incluso) fino alla fine della stringa utilizzando come indice finale la lunghezza totale della stringa originale.

## Approfondimento

Oltre ai parametri di inizio e fine, la funzione `substring()` può anche accettare un singolo parametro di inizio. In questo caso, verrà restituita la sottostringa che inizia dall'indice specificato fino alla fine della stringa.

```
Kotlin val str = "Ciao a tutti!" val subs = str.substring(5) println(subs)
```

In questo esempio, la sottostringa restituita sarebbe `a tutti!`.

Inoltre, il metodo `substring()` può anche essere utilizzato per ottenere una copia della stringa originale. Basta specificare gli stessi indici di inizio e fine o solo l'indice di inizio.

```
Kotlin val str = "Ciao a tutti!" val subs = str.substring(0,13) println(subs == str) // restituirà true val newStr = str.substring(0) println(newStr == str) // restituirà true
```

Questo ci permette di utilizzare la funzione `substring()` per effettuare una copia della nostra stringa originale e lavorarci senza cambiare la stringa originale stessa.

## Vedi Anche

- [Documentazione ufficiale Kotlin su `substring()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Funzioni di Stringa utili in Kotlin](https://cleancodestudio.com/tutorials/kotlin/string-functions-part1/)

Grazie per aver letto questo articolo! Speriamo che ora tu abbia una migliore comprensione di come estrarre delle sottostringhe in Kotlin e come utilizzarle per manipolare i dati nelle tue applicazioni. Continua a seguire il nostro blog per ulteriori tutorial su Kotlin e altre tecnologie di sviluppo. A presto!