---
title:                "Ottenere la data corrente"
html_title:           "Swift: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

È arrivato il momento di parlare di qualcosa di molto importante per i programmatori: la data corrente. Esatto, quella che compare sulla barra delle applicazioni del tuo dispositivo. Che sia un'app per il tuo cellulare o un software sul tuo computer, la possibilità di ottenere la data attuale è un'operazione fondamentale per molti sviluppatori. Ma perché è così importante e come possiamo ottenerla? Scopriamolo insieme nella nostra guida in stile "a scuola di informatica".

## Cos'è e perché è importante?

"Ottenere la data corrente" è semplicemente il processo di trovare e visualizzare la data attuale. Perché i programmatori lo fanno? Beh, ci sono molte ragioni! Ad esempio, potresti voler mostrare la data attuale ai tuoi utenti sull'interfaccia di un'app o utilizzarla per calcolare il tempo trascorso da un determinato evento. Inoltre, in certi casi potrebbe essere necessario utilizzarla come valida chiave per un sistema di autenticazione.

## Come fare:

Per ottenere la data corrente in Swift, possiamo utilizzare la classe Date e il suo metodo init() che restituisce una data con il momento attuale impostato. Ad esempio:

```Swift
let currentDate = Date()
print(currentDate)
```
Output: 2021-08-09 16:30:00 +0000

Come puoi vedere, il formato di output è leggermente diverso dal classico formato con la data e l'ora separati da uno spazio. Ma non preoccuparti, possiamo risolvere facilmente questa cosa utilizzando una classe chiamata DateFormatter. Questa classe ci permette di formattare la data secondo le nostre preferenze. Ad esempio:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yy"
print(dateFormatter.string(from: currentDate))
```
Output: 09.08.21

Come puoi vedere, abbiamo modificato il formato della data utilizzando la proprietà dateFormat della nostra istanza di DateFormatter e abbiamo utilizzato il metodo string(from:) per ottenere la data formattata.

## Approfondimento:

Se hai già programmato in linguaggi come Java o C++, potresti sapere che Date è in realtà una classe predefinita di questi linguaggi. Tuttavia, in Swift è una struct, ovvero un tipo di dato che contiene un insieme di proprietà e metodi. Inoltre, quando viene creato un'istanza di Date, non rappresenta mai una data specifica ma sempre la data e l'ora attuali.

Oltre all'utilizzo della classe Date, ci sono molte altre alternative per ottenere la data corrente in Swift. Ad esempio, è possibile utilizzare il framework Foundation o il framework Calendar, il quale ci permette di manipolare le date aggiungendo o sottraendo componenti come giorni, ore o minuti. Inoltre, alcune librerie di terze parti, come DateTools o SwifterSwift, offrono funzionalità avanzate per gestire le date correnti.

## Vedi anche:

Speriamo che questa guida ti sia stata utile per comprendere come ottenere la data corrente in Swift. Per ulteriori informazioni sulla classe Date e su come gestire le date in generale, puoi consultare la documentazione ufficiale di Apple su [Date](https://developer.apple.com/documentation/foundation/date). Inoltre, puoi approfondire il tema leggendo questo [articolo](https://www.hackingwithswift.com/example-code/dates-and-times/how-to-convert-dates-and-times-to-a-string-using-dateformatter) sul sito Hacking with Swift. Buon coding!