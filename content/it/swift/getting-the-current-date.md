---
title:    "Swift: Ottieni la data corrente."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, avrai sicuramente la necessità di ottenere la data corrente in una tua applicazione. Che si tratti di mostrare la data in una label o di utilizzarla per eseguire un'azione specifica, ottenere la data corrente è un'operazione fondamentale in molti scenari di programmazione.

## Come fare

Per ottenere la data corrente in Swift, puoi utilizzare la classe `Date` e il metodo `init()` che restituisce un oggetto di tipo `Date` con la data e l'ora correnti. Ad esempio:

```Swift
let currentDate = Date()
// Output: 2021-05-24 12:00:00 +0000 
```

In questo modo, abbiamo ottenuto la data corrente con l'ora UTC. Se invece vogliamo mostrare la data e l'ora in una specifica zona di fuso orario, dobbiamo utilizzare uno `TimeZone` e un` DateFormatter` per formattare la data come desideriamo.

```Swift
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy HH:mm:ss"
dateFormatter.timeZone = TimeZone(abbreviation: "UTC+1")
// Output: 24/05/2021 13:00:00 
```

## Approfondimento

Il metodo `init()` della classe `Date` in realtà richiama il metodo `init(timeIntervalSinceReferenceDate: TimeInterval)` che restituisce un oggetto `Date` in base al tempo trascorso dal 1 gennaio 2001 a mezzanotte UTC. Questo valore è rappresentato dalla variabile `TimeInterval` che è essenzialmente un alias per `Double`, quindi possiamo anche passare manualmente il tempo trascorso come parametro per creare un oggetto `Date` specifico. Ad esempio:

```Swift
let timestamp = TimeInterval(86400 * 7) // calcola il numero di secondi in una settimana
let date = Date(timeIntervalSinceReferenceDate: timestamp)
// Output: 2001-01-08 00:00:00 +0000 
```

In questo modo, possiamo ottenere la data di una settimana fa, o in generale una data specifica in base al tempo trascorso.

## Vedi anche

- [Documentazione ufficiale di Swift su Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial: Come ottenere la data corrente in Swift](https://www.youtube.com/watch?v=UzGuFkxWhk4)
- [Articolo: Le diverse opzioni per gestire le date in Swift](https://www.hackingwithswift.com/articles/161/how-to-work-with-dates-and-times-in-swift)