---
title:                "Interpolazione di una stringa"
html_title:           "Swift: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

Che cos'è & perché?

L'interpolazione di stringhe è una tecnica utilizzata dai programmatori per creare una stringa dinamica utilizzando variabili o espressioni all'interno di un'altra stringa. Questo ci aiuta a creare codice più flessibile e leggibile, in particolare quando dobbiamo combinare informazioni diverse per creare una stringa unica.

Come:

```
let name = "Marco"
let age = 28

print("Ciao, mi chiamo \(name) e ho \(age) anni.")
```

Output: Ciao, mi chiamo Marco e ho 28 anni.

In questo esempio, stiamo utilizzando due variabili, "name" e "age", all'interno di una stringa per creare un messaggio personalizzato. Nota l'uso di "\(...)" per indicare dove inserire le variabili all'interno della stringa.

Esistono diverse varianti di interpolazione di stringhe in Swift, tra cui:

- Interpolazione di stringhe semplice: come nell'esempio sopra, utilizziamo "\(...)" per includere variabili all'interno di una stringa.
- Interpolazione di stringhe multilinea: utilizzando gli apici tripli ``` al posto dei soliti " o ' per creare una stringa su più righe con l'interpolazione di stringhe.
- Interpolazione di stringhe personalizzata: utilizzando il formato ```String(format: , )``` possiamo creare una stringa con una configurazione personalizzata utilizzando placeholder o formattazione di numeri decimali.

Che cos'è un placeholder? Sono degli spazi vuoti all'interno della stringa in cui possiamo inserire i nostri valori in modo dinamico. Ad esempio:

```
let letter = "Ho ricevuto un punteggio di %d su %d nel mio esame di matematica."
let grade = 85
let total = 100

let message = String(format: letter, grade, total)

print(message)
```

Output: Ho ricevuto un punteggio di 85 su 100 nel mio esame di matematica.

In questo caso, stiamo utilizzando i placeholder "%d" per indicare dove inserire i nostri valori nella stringa di formato. Ciò ci consente di utilizzare una sola stringa per creare messaggi personalizzati in base ai dati specifici che abbiamo. Possiamo anche formattare la stringa di output nel modo che preferiamo, come mostrato nell'esempio.

Deep Dive:

L'interpolazione di stringhe è stata introdotta in Swift 2 ed è diventata il metodo preferito per creare stringhe dinamiche rispetto alle vecchie tecniche come la concatenazione e l'utilizzo del metodo "stringWithFormat".

Se non si desidera utilizzare l'interpolazione di stringhe, è comunque possibile creare una stringa dinamica usando la concatenazione di stringhe e variabili o utilizzando il metodo "stringWithFormat". Tuttavia, l'interpolazione di stringhe è considerata più efficiente e leggibile in questi casi.

Vale la pena notare che, se vogliamo utilizzare l'interpolazione di stringhe all'interno di una stringa di template, dobbiamo utilizzare l'apice inverso, "`", al posto di " nella nostra stringa di base. Ad esempio:

```
let price = 10

print("Il totale del tuo ordine è \(price) euro.")
```

See Also:

Per ulteriori informazioni sull'interpolazione di stringhe in Swift, puoi consultare la documentazione ufficiale di Apple o questo articolo di NSHipster (in inglese): https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293

Se vuoi esplorare altre tecniche per creare stringhe dinamiche in Swift, puoi anche leggere su come utilizzare "string interpolation" in questo articolo di Hacking with Swift (in inglese): https://www.hackingwithswift.com/articles/179/how-to-use-string-interpolation-in-swift