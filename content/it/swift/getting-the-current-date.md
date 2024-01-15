---
title:                "How to Get the Current Date"
html_title:           "Swift: How to Get the Current Date"
simple_title:         "How to Get the Current Date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori di software utilizzano la data corrente come parte delle loro applicazioni, sia per logica di business che per motivi di user experience. In questo articolo, scoprirai come ottenere la data corrente utilizzando Swift, il linguaggio di programmazione utilizzato per creare applicazioni per iOS, macOS e altri sistemi operativi.

## Come

Per ottenere la data corrente in Swift, è possibile utilizzare l'oggetto `Date` della libreria standard. Vediamo un esempio:

```Swift
let now = Date()
```

In questo caso, abbiamo creato un'istanza dell'oggetto `Date` chiamata `now`, che rappresenta la data e l'ora correnti. Per visualizzare la data in un formato leggibile, possiamo utilizzare la classe `DateFormatter` e il metodo `string(from:)` per formattare l'oggetto `Date` in una stringa. Vediamo come:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy" // formato desiderato per la data
let stringDate = formatter.string(from: now)

print(stringDate) // output: "21/10/2021" (in base alla data corrente)
```

In questo caso, abbiamo creato un'istanza di `DateFormatter` e abbiamo specificato il formato desiderato per la data tramite la proprietà`dateFormat`. Quindi, abbiamo utilizzato il metodo `string(from:)` per formattare l'oggetto Date in una stringa. È possibile scegliere tra vari formati di data, come mostrato nella documentazione ufficiale di Apple (link nella sezione "See Also").

Inoltre, è possibile ottenere informazioni più dettagliate sulla data corrente utilizzando il `Calendar` di Swift. Vediamo un esempio:

```Swift
let calendar = Calendar.current
let year = calendar.component(.year, from: now)
let month = calendar.component(.month, from: now)
let day = calendar.component(.day, from: now)

print("\(day)/\(month)/\(year)") // output: "21/10/2021" (in base alla data corrente)
```

In questo caso, abbiamo utilizzato il `Calendar` di Swift per estrarre i singoli componenti della data e visualizzarli in un formato personalizzato.

## Deep Dive

Se vuoi ottenere la data corrente in un fuso orario specifico, puoi utilizzare i `TimeZone` di Swift. Vediamo un esempio:

```Swift
let timeZone = TimeZone(identifier: "Europe/Rome") // specifica il fuso orario desiderato
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy HH:mm:ss zzz" // formato desiderato per la data e l'ora

// impostiamo la timezone del nostro formatter
formatter.timeZone = timeZone

let stringDate = formatter.string(from: now)

print(stringDate) // output: "21/10/2021 12:36:25 CEST" (in base alla data e l'ora correnti)
```

In questo caso, abbiamo creato un'istanza della classe `TimeZone` specificando il fuso orario desiderato e lo abbiamo utilizzato per formattare l'oggetto `Date` tramite il nostro `DateFormatter`. 

Inoltre, puoi anche utilizzare altre utility di Swift come `Calendar` e `DateComponents` per eseguire operazioni avanzate sulla data corrente, come ottenere il primo e l'ultimo giorno del mese, o aggiungere un certo numero di giorni alla data corrente.

## See Also

- Documentazione ufficiale di Apple su come lavorare con le date in Swift: https://developer.apple.com/documentation/foundation/date
- Tutorial su come utilizzare Date e DateFormatter in Swift: https://www.hackingwithswift.com/example-code/system/how-to-create-a-date-picker-and-read-values-from-it
- Ulteriori informazioni su Calendar e TimeZone in Swift: https://www.avanderlee.com/swift/nsdatecomponents-date-calculations/

(Questo articolo è stato scritto utilizzando Swift 5.4 e Xcode 12.5.1)